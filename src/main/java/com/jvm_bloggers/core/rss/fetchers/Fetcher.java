package com.jvm_bloggers.core.rss.fetchers;

import com.rometools.rome.feed.synd.SyndFeed;

import javaslang.control.Option;

public interface Fetcher {

    Option<SyndFeed> fetch(String rssUrl);
}
