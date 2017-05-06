package com.jvm_bloggers.core.rss;

import com.jvm_bloggers.core.rss.fetchers.HttpFetcher;
import com.jvm_bloggers.core.rss.fetchers.HttpFetcherWithoutSslVerification;
import com.jvm_bloggers.core.rss.fetchers.WgetFetcher;
import com.jvm_bloggers.core.utils.Validators;
import com.rometools.rome.feed.synd.SyndFeed;

import javaslang.control.Option;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.stereotype.Component;

@Component
@Slf4j
@RequiredArgsConstructor
public class SyndFeedProducer {

    private final HttpFetcher httpFetcher;
    private final HttpFetcherWithoutSslVerification httpFetcherWithoutSslVerification;
    private final WgetFetcher wgetFetcher;

    /**
     * Try to fetch rss from given url.
     * 1st attempt - standard https connection
     * 2nd attempt - fallback for some self-signed certificates using https connection with
     * disabled SSL certificate checking
     * 3rd attempt - execute wget unix command, save as rss a file in tmp directory
     * and create SyndFeed from it
     */
    public Option<SyndFeed> createFor(String rssUrl) {
        return httpFetcher.fetch(rssUrl)
            .orElse(() -> httpFetcherWithoutSslVerification.fetch(rssUrl))
            .orElse(() -> wgetFetcher.fetch(rssUrl));
    }

    public Option<String> validUrlFromRss(String rss) {
        Option<String> url = createFor(rss).map(SyndFeed::getLink);
        return url.filter(Validators::isUrlValid);
    }
}
