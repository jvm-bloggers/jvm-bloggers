package com.jvm_bloggers.core.rss

import com.jvm_bloggers.core.rss.fetchers.HttpFetcher
import com.jvm_bloggers.core.rss.fetchers.HttpFetcherWithoutSslVerification
import com.jvm_bloggers.core.rss.fetchers.WgetFetcher
import com.rometools.rome.feed.synd.SyndFeed
import javaslang.control.Option
import spock.lang.Specification
import spock.lang.Subject

class SyndFeedProducerSpec extends Specification {

    private final HttpFetcher httpFetcher = Stub()
    private final HttpFetcherWithoutSslVerification httpFetcherWithoutSslVerification = Stub()
    private final WgetFetcher wgetFetcher = Stub()
    private final SyndFeed syndFeed = Mock()

    static final String URL = "URL"

    @Subject
    SyndFeedProducer feedProducer = new SyndFeedProducer(httpFetcher, httpFetcherWithoutSslVerification, wgetFetcher)

    def "Should fetch through primary fetcher"() {
        given:
            httpFetcher.fetch(URL) >> Option.of(syndFeed)

        when:
            def actual = feedProducer.createFor(URL)

        then:
            actual.isDefined()
            actual.get() == syndFeed
    }

    def "Should fallback to second fetcher"() {
        given:
            httpFetcher.fetch(URL) >> Option.none()
            httpFetcherWithoutSslVerification.fetch(URL) >> Option.of(syndFeed)

        when:
            def actual = feedProducer.createFor(URL)

        then:
            actual.isDefined()
            actual.get() == syndFeed
    }

    def "Should fallback to any fetcher"() {
        given:
            httpFetcher.fetch(URL) >> Option.none()
            httpFetcherWithoutSslVerification.fetch(URL) >> Option.none()
            wgetFetcher.fetch(URL) >> Option.of(syndFeed)

        when:
            def actual = feedProducer.createFor(URL)

        then:
            actual.isDefined()
            actual.get() == syndFeed
    }
}
