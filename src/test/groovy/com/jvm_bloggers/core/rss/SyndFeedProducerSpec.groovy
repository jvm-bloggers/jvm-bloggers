package com.jvm_bloggers.core.rss

import com.jvm_bloggers.core.rss.fetchers.HttpFetcher
import com.jvm_bloggers.core.rss.fetchers.HttpFetcherWithoutSslVerification
import com.jvm_bloggers.core.rss.fetchers.WgetFetcher
import com.rometools.rome.feed.synd.SyndFeed
import javaslang.control.Option
import spock.lang.Specification
import spock.lang.Subject

class SyndFeedProducerSpec extends Specification {

    private final HttpFetcher httpFetcher = Mock()
    private final HttpFetcherWithoutSslVerification httpFetcherWithoutSslVerification = Mock()
    private final WgetFetcher wgetFetcher = Mock()
    private final SyndFeed syndFeed = Mock()

    static final String URL = "URL"

    @Subject
    SyndFeedProducer feedProducer = new SyndFeedProducer(Arrays.asList(httpFetcher, httpFetcherWithoutSslVerification, wgetFetcher))

    def "Should fetch through primary fetcher"() {
        given:
            httpFetcher.fetch(URL) >> Option.of(syndFeed)

        when:
            def actual = feedProducer.createFor(URL)

        then:
            actual.isDefined()
            actual.get() == syndFeed
        and:
            0 * httpFetcherWithoutSslVerification.fetch(URL)
            0 * wgetFetcher.fetch(URL)
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
        and:
            0 * wgetFetcher.fetch(URL)
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

    def "Should return Option.none when all methods fail"() {
        given:
            httpFetcher.fetch(URL) >> Option.none()
            httpFetcherWithoutSslVerification.fetch(URL) >> Option.none()
            wgetFetcher.fetch(URL) >> Option.none()

        when:
            def actual = feedProducer.createFor(URL)

        then:
            actual.isEmpty()
    }
}
