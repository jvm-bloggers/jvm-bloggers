package com.jvm_bloggers.core.rss;

import com.jvm_bloggers.core.rss.json.RssToJsonConverter;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.SyndFeedOutput;

import lombok.SneakyThrows;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.io.PrintWriter;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import static com.google.common.base.MoreObjects.firstNonNull;

@RestController
public class BlogPostsController {
    public static final String RSS_FEED_MAPPING = "/pl/rss";

    private final AggregatedRssFeedProducer rssProducer;
    private final RssToJsonConverter rssToJsonConverter;

    private final int defaultLimit;

    @Autowired
    public BlogPostsController(AggregatedRssFeedProducer rssProducer,
                               RssToJsonConverter rssToJsonConverter,
                               @Value("${generated.rss.entries.limit}") int defaultLimit) {
        this.rssProducer = rssProducer;
        this.rssToJsonConverter = rssToJsonConverter;
        this.defaultLimit = defaultLimit;
    }

    @SneakyThrows
    @RequestMapping(RSS_FEED_MAPPING)
    public void getRss(HttpServletRequest request, HttpServletResponse response,
                       PrintWriter writer,
                       @RequestParam(required = false) Integer limit,
                       @RequestParam(required = false) Set<String> excludedAuthors,
                       @RequestParam(required = false, defaultValue = "xml") String format) {
        limit = firstNonNull(limit, defaultLimit);

        SyndFeed feed =
            rssProducer.getRss(request.getRequestURL().toString(), limit, excludedAuthors);

        if (SupportedFormat.XML.toString().equalsIgnoreCase(format)) {
            response.setContentType(MediaType.APPLICATION_ATOM_XML_VALUE);

            new SyndFeedOutput().output(feed, writer);
        } else if (SupportedFormat.JSON.toString().equalsIgnoreCase(format)) {
            response.setContentType(MediaType.APPLICATION_JSON_VALUE);

            rssToJsonConverter.convert(feed).write(writer);
        } else {
            response.sendError(HttpStatus.BAD_REQUEST.value(), "Unsupported format type!");
        }
    }

    /**
     * Supported RSS feed formats
     */
    enum SupportedFormat {
        XML,
        JSON
    }
}
