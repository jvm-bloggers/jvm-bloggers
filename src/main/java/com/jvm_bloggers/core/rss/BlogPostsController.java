package com.jvm_bloggers.core.rss;

import com.jvm_bloggers.core.rss.json.SyndFeedToJsonConverter;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.SyndFeedOutput;

import lombok.SneakyThrows;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import static com.google.common.base.MoreObjects.firstNonNull;
import static com.jvm_bloggers.core.rss.BlogPostsController.SupportedRssFormat.JSON;
import static com.jvm_bloggers.core.rss.BlogPostsController.SupportedRssFormat.XML;
import static javaslang.API.$;
import static javaslang.API.Case;
import static javaslang.API.Match;
import static javaslang.API.run;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.MediaType.APPLICATION_ATOM_XML_VALUE;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;

@RestController
public class BlogPostsController {
    public static final String RSS_FEED_MAPPING = "/pl/rss";

    private final AggregatedRssFeedProducer rssProducer;
    private final SyndFeedToJsonConverter syndFeedToJsonConverter;

    private final Integer defaultLimit;

    @Autowired
    public BlogPostsController(AggregatedRssFeedProducer rssProducer,
                               SyndFeedToJsonConverter syndFeedToJsonConverter,
                               @Value("${generated.rss.entries.limit}") Integer defaultLimit) {
        this.rssProducer = rssProducer;
        this.syndFeedToJsonConverter = syndFeedToJsonConverter;
        this.defaultLimit = defaultLimit;
    }

    @SneakyThrows
    @RequestMapping(RSS_FEED_MAPPING)
    public void getRss(HttpServletRequest request, HttpServletResponse response,
                       PrintWriter writer,
                       @RequestParam(required = false) Integer limit,
                       @RequestParam(required = false) Set<String> excludedAuthors,
                       @RequestParam(required = false, defaultValue = "xml") String format) {
        Integer checkedLimit = firstNonNull(limit, defaultLimit);

        SyndFeed feed =
            rssProducer.getRss(request.getRequestURL().toString(), checkedLimit, excludedAuthors);

        Match(format).of(
            Case(XML::sameAs, f -> run(() -> prepareXmlResponse(response, feed, writer))),
            Case(JSON::sameAs, f -> run(() -> prepareJsonResponse(response, feed, writer))),
            Case($(), f -> run(() -> prepareBadRequestResponse(response, f)))
        );
    }

    private void prepareJsonResponse(HttpServletResponse response, SyndFeed feed,
                                     PrintWriter writer) {
        response.setContentType(APPLICATION_JSON_VALUE);
        syndFeedToJsonConverter.convert(feed).write(writer);
    }

    @SneakyThrows
    private void prepareXmlResponse(HttpServletResponse response, SyndFeed feed,
                                    PrintWriter writer) {
        response.setContentType(APPLICATION_ATOM_XML_VALUE);
        new SyndFeedOutput().output(feed, writer);
    }

    @SneakyThrows
    private void prepareBadRequestResponse(HttpServletResponse response, String format) {
        response.sendError(BAD_REQUEST.value(),
            String.format("'%s' is unsupported format type! Supported are only '%s'",
                format, Arrays.toString(SupportedRssFormat.values())));
    }

    enum SupportedRssFormat {
        XML,
        JSON;

        boolean sameAs(String format) {
            return toString().equalsIgnoreCase(format);
        }
    }
}
