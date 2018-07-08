package com.jvm_bloggers.core.rss;

import com.rometools.rome.feed.synd.SyndFeed;

import lombok.RequiredArgsConstructor;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import static org.springframework.http.MediaType.APPLICATION_ATOM_XML_VALUE;
import static org.springframework.http.MediaType.APPLICATION_JSON_UTF8_VALUE;

@RestController
@RequiredArgsConstructor
public class BlogPostsController {
    private static final String DEPRECATED_BLOGS_FEED = "/pl/rss";
    public static final String BLOGS_FEED = "/feed/blogs";
    public static final String ISSUES_FEED = "/feed/issues";

    private final AggregatedRssFeedProducer blogRssProducer;
    private final IssuesRssFeedProducer issuesRssProducer;

    @RequestMapping(
        method = RequestMethod.GET,
        path = {BLOGS_FEED, DEPRECATED_BLOGS_FEED},
        produces = {APPLICATION_ATOM_XML_VALUE, APPLICATION_JSON_UTF8_VALUE}
    )
    public SyndFeed getBlogRss(
        HttpServletRequest request,
        @RequestParam(defaultValue = "${generated.rss.entries.limit}") Integer limit,
        @RequestParam(required = false) Set<String> excludedAuthors) {

        return blogRssProducer.getRss(request.getRequestURL().toString(), limit, excludedAuthors);
    }

    @RequestMapping(
        method = RequestMethod.GET,
        path = ISSUES_FEED,
        produces = {APPLICATION_ATOM_XML_VALUE, APPLICATION_JSON_UTF8_VALUE}
    )
    public SyndFeed getEntriesRss(
        HttpServletRequest request,
        @RequestParam(defaultValue = "${generated.rss.entries.limit}") Integer limit) {

        return issuesRssProducer.getRss(request.getRequestURL().toString(), limit);
    }
}
