package com.jvm_bloggers.core.rss.json;

import lombok.experimental.UtilityClass;

/**
 * Keys which are used in parsing RSS Feed to a JSON text content
 *
 * @author kraluk
 */
@UtilityClass
class RssJsonKey {
    static final String GENERATOR = "generator";
    static final String LINK = "link";
    static final String ENTRIES = "entries";
    static final String TITLE = "title";
    static final String DESCRIPTION = "description";
    static final String AUTHOR = "author";
    static final String DATE = "date";
}
