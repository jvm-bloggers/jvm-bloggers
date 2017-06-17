package com.jvm_bloggers.core.rss.json;

/**
 * Keys which are used in parsing RSS Feed to a JSON text content
 *
 * @author kraluk
 */
enum RssJsonKey {
    GENERATOR("generator"),
    LINK("link"),
    ENTRIES("entries"),
    TITLE("title"),
    DESCRIPTION("description"),
    AUTHOR("author"),
    DATE("date");

    private String key;

    RssJsonKey(String key) {
        this.key = key;
    }

    public String getKey() {
        return key;
    }
}
