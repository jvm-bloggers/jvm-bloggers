package com.jvm_bloggers.core.rss;

import com.rometools.rome.feed.synd.SyndFeed;

import lombok.RequiredArgsConstructor;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import static org.springframework.http.MediaType.APPLICATION_ATOM_XML_VALUE;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;

@RestController
@RequiredArgsConstructor
public class BlogPostsController {
    public static final String ENTRIES_RSS_FEED = "/pl/rss";
    public static final String ISSUES_RSS_FEED = "/pl/issues-rss";

    private final AggregatedRssFeedProducer blogRssProducer;
    private final IssuesRssFeedProducer issuesRssProducer;

    @GetMapping(
      path = { ENTRIES_RSS_FEED + ".xml", ENTRIES_RSS_FEED},
      produces = APPLICATION_ATOM_XML_VALUE
    )
    public SyndFeed getRssAxXml(
      HttpServletRequest request,
      @RequestParam(defaultValue = "${generated.rss.entries.limit}") Integer limit,
      @RequestParam(required = false) Set<String> excludedAuthors) {

        return blogRssProducer.getRss(request.getRequestURL().toString(), limit, excludedAuthors);
    }

    @GetMapping(
      path = ENTRIES_RSS_FEED + ".json",
      produces = APPLICATION_JSON_VALUE
    )
    public SyndFeed getRssAsJson(
      HttpServletRequest request,
      @RequestParam(defaultValue = "${generated.rss.entries.limit}") Integer limit,
      @RequestParam(required = false) Set<String> excludedAuthors) {

        return blogRssProducer.getRss(request.getRequestURL().toString(), limit, excludedAuthors);
    }

    @GetMapping(
      path = {ISSUES_RSS_FEED, ISSUES_RSS_FEED + ".xml" },
      produces = APPLICATION_ATOM_XML_VALUE
    )
    public SyndFeed getEntriesRss(
      HttpServletRequest request,
      @RequestParam(defaultValue = "${generated.rss.entries.limit}") Integer limit) {

        return issuesRssProducer.getRss(request.getRequestURL().toString(), limit);
    }

    @GetMapping(
      path = ISSUES_RSS_FEED + ".json",
      produces = { APPLICATION_JSON_VALUE}
    )
    public SyndFeed getEntriesRssAsJson(
      HttpServletRequest request,
      @RequestParam(defaultValue = "${generated.rss.entries.limit}") Integer limit) {

        return issuesRssProducer.getRss(request.getRequestURL().toString(), limit);
    }

}
