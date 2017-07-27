package com.jvm_bloggers.core.rss.fetchers;

import com.rometools.rome.feed.synd.SyndFeed;

import io.vavr.control.Try;

public interface RssFetcher {

    String FAKE_USER_AGENT = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) "
        + "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.1 Safari/537.36";

    Try<SyndFeed> fetch(String rssUrl);
}
