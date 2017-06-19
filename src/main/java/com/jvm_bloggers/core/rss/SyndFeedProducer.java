package com.jvm_bloggers.core.rss;

import com.jvm_bloggers.core.rss.fetchers.RssFetcher;
import com.jvm_bloggers.core.utils.Validators;
import com.rometools.rome.feed.synd.SyndFeed;

import javaslang.collection.Seq;
import javaslang.collection.Stream;
import javaslang.control.Option;
import javaslang.control.Try;

import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class SyndFeedProducer {

    private final Seq<RssFetcher> fetchers;

    @Autowired
    public SyndFeedProducer(java.util.List<RssFetcher> fetchers) {
        this.fetchers = Stream.ofAll(fetchers);
    }

    public Option<SyndFeed> createFor(String rssUrl) {
        return fetchers
            .map(fetcher -> fetcher.fetch(rssUrl))
            .find(Try::isSuccess)
            .flatMap(Try::getOption);
    }

    public Option<String> validUrlFromRss(String rss) {
        Option<String> url = createFor(rss).map(SyndFeed::getLink);
        return url.filter(Validators::isUrlValid);
    }

}
