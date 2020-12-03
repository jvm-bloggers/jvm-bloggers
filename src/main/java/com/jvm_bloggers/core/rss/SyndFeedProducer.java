package com.jvm_bloggers.core.rss;

import com.google.common.collect.Lists;
import com.jvm_bloggers.core.rss.fetchers.RssFetcher;
import com.jvm_bloggers.core.rss.fetchers.WgetRssFetcherWithIllegalCharsEscaper;
import com.jvm_bloggers.core.utils.Validators;
import com.rometools.rome.feed.synd.SyndFeed;
import io.vavr.collection.List;
import io.vavr.collection.Seq;
import io.vavr.collection.Stream;
import io.vavr.control.Option;
import io.vavr.control.Try;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.stream.Collectors;

@Component
@Slf4j
public class SyndFeedProducer {

    private final Seq<RssFetcher> fetchers;

    @Autowired
    public SyndFeedProducer(java.util.List<RssFetcher> fetchers) {
        this.fetchers = Stream.ofAll(fetchers);
        log.info(
            "Creating {} with following fetchers {}",
            this.getClass().getSimpleName(),
            stringifyFetcherClassNames(fetchers)
        );
    }

    private String stringifyFetcherClassNames(java.util.List<RssFetcher> fetchers) {
        return List
            .ofAll(fetchers).map(f -> f.getClass().getSimpleName())
            .collect(Collectors.joining(", "));
    }

    public Option<SyndFeed> createFor(String rssUrl) {
        Option<SyndFeed> syndFeed = fetchers
            .map(fetcher -> fetcher.fetch(rssUrl))
            .find(Try::isSuccess)
            .flatMap(Try::toOption);
        if (syndFeed.isEmpty()) {
            log.warn("Error: Unable to fetch RSS for {}", rssUrl);
        }
        return syndFeed;
    }

    public Option<String> validUrlFromRss(String rss) {
        Option<String> url = createFor(rss).map(SyndFeed::getLink);
        return url.filter(Validators::isUrlValid);
    }

    // Helper method to test how troublesome RSS feeds are behaving
    public static void main(String[] args) {
        SyndFeedProducer feedProducer = new SyndFeedProducer(
            Lists.newArrayList(
                new WgetRssFetcherWithIllegalCharsEscaper()
            )
        );

        String rssUrl = "https://wrrathy.github.io/feed.xml";

        Option<SyndFeed> syndFeed = feedProducer.createFor(rssUrl);
        System.out.println("Rss = " + rssUrl);
        System.out.println("Url = " + syndFeed.get().getLink());
        System.out.println("Url is valid = " + Validators.isUrlValid(syndFeed.get().getLink()));
        System.out.println("Number of articles in feed = " + syndFeed.get().getEntries().size());
    }

}
