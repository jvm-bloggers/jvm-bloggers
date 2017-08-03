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
    public static final String RSS_FEED_MAPPING = "/pl/rss";

    private final AggregatedRssFeedProducer rssProducer;

    @RequestMapping(
        method = RequestMethod.GET,
        path = RSS_FEED_MAPPING,
        produces = {APPLICATION_ATOM_XML_VALUE, APPLICATION_JSON_UTF8_VALUE}
    )
    public SyndFeed getRss(
        HttpServletRequest request,
        @RequestParam(defaultValue = "${generated.rss.entries.limit}") Integer limit,
        @RequestParam(required = false) Set<String> excludedAuthors) {

        return rssProducer.getRss(request.getRequestURL().toString(), limit, excludedAuthors);
    }
}
