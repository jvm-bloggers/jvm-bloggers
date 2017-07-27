package com.jvm_bloggers.core.rss;

import com.rometools.rome.feed.synd.SyndFeed;

import lombok.SneakyThrows;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import static com.google.common.base.MoreObjects.firstNonNull;
import static org.springframework.http.MediaType.APPLICATION_ATOM_XML_VALUE;
import static org.springframework.http.MediaType.APPLICATION_JSON_UTF8_VALUE;

@RestController
public class BlogPostsController {
    public static final String RSS_FEED_MAPPING = "/pl/rss";

    private final AggregatedRssFeedProducer rssProducer;

    private final Integer defaultLimit;

    @Autowired
    public BlogPostsController(AggregatedRssFeedProducer rssProducer,
                               @Value("${generated.rss.entries.limit}") Integer defaultLimit) {
        this.rssProducer = rssProducer;
        this.defaultLimit = defaultLimit;
    }

    @SneakyThrows
    @RequestMapping(
        method = RequestMethod.GET,
        path = RSS_FEED_MAPPING,
        produces = {APPLICATION_ATOM_XML_VALUE, APPLICATION_JSON_UTF8_VALUE}
    )
    public SyndFeed getRss(HttpServletRequest request,
                           @RequestParam(required = false) Integer limit,
                           @RequestParam(required = false) Set<String> excludedAuthors) {
        Integer checkedLimit = firstNonNull(limit, defaultLimit);

        return
            rssProducer.getRss(request.getRequestURL().toString(), checkedLimit, excludedAuthors);
    }
}
