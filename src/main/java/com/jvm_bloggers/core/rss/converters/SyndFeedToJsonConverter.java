package com.jvm_bloggers.core.rss.converters;

import com.github.openjson.JSONObject;
import com.rometools.rome.feed.synd.SyndEntry;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.feed.synd.SyndLink;

import io.vavr.collection.Stream;
import io.vavr.control.Option;

import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

import static com.jvm_bloggers.core.rss.AggregatedRssFeedProducer.SELF_REL;
import static com.jvm_bloggers.core.rss.converters.SyndFeedToJsonConverter.RssJsonKey.AUTHOR;
import static com.jvm_bloggers.core.rss.converters.SyndFeedToJsonConverter.RssJsonKey.DATE;
import static com.jvm_bloggers.core.rss.converters.SyndFeedToJsonConverter.RssJsonKey.DESCRIPTION;
import static com.jvm_bloggers.core.rss.converters.SyndFeedToJsonConverter.RssJsonKey.ENTRIES;
import static com.jvm_bloggers.core.rss.converters.SyndFeedToJsonConverter.RssJsonKey.GENERATOR;
import static com.jvm_bloggers.core.rss.converters.SyndFeedToJsonConverter.RssJsonKey.LINK;
import static com.jvm_bloggers.core.rss.converters.SyndFeedToJsonConverter.RssJsonKey.TITLE;
import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_TIME_FORMATTER;
import static com.jvm_bloggers.utils.ZoneTimeProvider.DEFAULT_ZONE;

@Component
@Slf4j
class SyndFeedToJsonConverter {

    private final String baseUrl;

    @Autowired
    public SyndFeedToJsonConverter(@Value("${application.baseUrl}") String baseUrl) {
        this.baseUrl = baseUrl;
    }

    private static JSONObject toJson(SyndFeed feed) {
        JSONObject json = new JSONObject();
        json.put(TITLE, feed.getTitle());
        json.put(DESCRIPTION, feed.getDescription());
        json.put(ENTRIES, toJson(feed.getEntries()));
        setLinkIfPresent(feed, json);
        return json;
    }

    private static List<JSONObject> toJson(List<SyndEntry> entries) {
        return entries.stream()
            .map(SyndFeedToJsonConverter::toJson)
            .collect(Collectors.toList());
    }

    private static JSONObject toJson(SyndEntry entry) {
        JSONObject json = new JSONObject();

        json.put(LINK, entry.getLink());
        json.put(TITLE, entry.getTitle());
        json.put(AUTHOR, entry.getAuthor());

        Option.of(entry.getDescription())
            .peek(d -> json.put(DESCRIPTION, d.getValue()));

        String date = DATE_TIME_FORMATTER.format(
            LocalDateTime.ofInstant(entry.getPublishedDate().toInstant(), DEFAULT_ZONE));

        json.put(DATE, date);
        return json;
    }

    private static void setLinkIfPresent(SyndFeed feed, JSONObject json) {
        Stream.ofAll(feed.getLinks())
            .find(link -> SELF_REL.equals(link.getRel()))
            .map(SyndLink::getHref)
            .forEach(link -> json.put(LINK, link));
    }

    public JSONObject convert(SyndFeed feed) {
        log.debug("Building JSON from the RSS feed...");

        JSONObject json = toJson(feed);
        json.put(GENERATOR, baseUrl);

        log.debug("JSON content generated successfully with '{}' entries",
            feed.getEntries().size());
        return json;
    }

    /**
     * Keys which are used in parsing RSS Feed to a JSON text content
     */
    @UtilityClass
    static class RssJsonKey {
        static final String GENERATOR = "generator";
        static final String LINK = "link";
        static final String ENTRIES = "entries";
        static final String TITLE = "title";
        static final String DESCRIPTION = "description";
        static final String AUTHOR = "author";
        static final String DATE = "date";
    }
}
