package com.jvm_bloggers.core.rss.json;

import com.rometools.rome.feed.synd.SyndEntry;
import com.rometools.rome.feed.synd.SyndFeed;

import javaslang.control.Option;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.ajax.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import static com.jvm_bloggers.core.rss.json.RssJsonKey.AUTHOR;
import static com.jvm_bloggers.core.rss.json.RssJsonKey.DATE;
import static com.jvm_bloggers.core.rss.json.RssJsonKey.DESCRIPTION;
import static com.jvm_bloggers.core.rss.json.RssJsonKey.ENTRIES;
import static com.jvm_bloggers.core.rss.json.RssJsonKey.GENERATOR;
import static com.jvm_bloggers.core.rss.json.RssJsonKey.LINK;
import static com.jvm_bloggers.core.rss.json.RssJsonKey.TITLE;
import static com.jvm_bloggers.utils.NowProvider.DEFAULT_ZONE;

/**
 * Simple {@link SyndFeed} objects to JSON plain text converter
 *
 * @author kraluk
 */
@Component
@Slf4j
public class RssToJsonConverter {
    static final DateTimeFormatter DATE_FORMATTER =
        DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");

    private final String baseUrl;

    @Autowired
    public RssToJsonConverter(@Value("${application.baseUrl}") String baseUrl) {
        this.baseUrl = baseUrl;
    }

    private static JSONObject toJson(SyndFeed feed) {
        JSONObject json = new JSONObject();

        json.put(TITLE.getKey(), feed.getTitle());
        json.put(LINK.getKey(), feed.getLink());
        json.put(DESCRIPTION.getKey(), feed.getDescription());
        json.put(ENTRIES.getKey(), toJson(feed.getEntries()));

        return json;
    }

    private static List<JSONObject> toJson(List<SyndEntry> entries) {
        List<JSONObject> result = new ArrayList<>(entries.size());

        entries.forEach(entry -> result.add(toJson(entry)));

        return result;
    }

    private static JSONObject toJson(SyndEntry entry) {
        JSONObject json = new JSONObject();

        json.put(LINK.getKey(), entry.getLink());
        json.put(TITLE.getKey(), entry.getTitle());
        json.put(AUTHOR.getKey(), entry.getAuthor());

        // Description
        Option.of(entry.getDescription())
            .peek(d -> json.put(DESCRIPTION.getKey(), d.getValue()));

        // Date
        String date = DATE_FORMATTER.format(
            LocalDateTime.ofInstant(entry.getPublishedDate().toInstant(), DEFAULT_ZONE));

        json.put(DATE.getKey(), date);

        return json;
    }

    /**
     * Converts complete {@link SyndFeed} instance to a JSON form
     *
     * @param feed a {@link SyndFeed} instance
     * @return a {@link JSONObject} containing complete JSONfied feed
     */
    public JSONObject convert(SyndFeed feed) {
        log.debug("Building JSON from the RSS feed...");

        JSONObject json = toJson(feed);
        json.put(GENERATOR.getKey(), baseUrl);
        json.put(LINK.getKey(), baseUrl + "/pl/rss");

        json.put(ENTRIES.getKey(), toJson(feed.getEntries()));

        log.debug("JSON content generated successfully with '{}' entries",
            feed.getEntries().size());
        return json;
    }
}
