package com.jvm_bloggers.core.rss.fetchers;

import com.rometools.rome.feed.synd.SyndFeed;

import javaslang.control.Try;

public interface Fetcher {

    Try<SyndFeed> fetch(String rssUrl);
}
