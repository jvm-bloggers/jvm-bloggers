package com.jvm_bloggers.core.rss

import com.jvm_bloggers.core.rss.fetchers.Fetcher
import com.rometools.rome.feed.synd.SyndFeed
import javaslang.control.Try
import spock.lang.Specification
import spock.lang.Subject

class SyndFeedProducerSpec extends Specification {

    private final Fetcher primaryFetcher = Mock()
    private final Fetcher fallbackFetcher = Mock()
    private final Fetcher lastFallbackFetcher = Mock()
    private final SyndFeed syndFeed = Mock()

    static final String URL = "URL"

    @Subject
    SyndFeedProducer feedProducer = new SyndFeedProducer(Arrays.asList(primaryFetcher, fallbackFetcher, lastFallbackFetcher))
    private Try<SyndFeed> failure = Try.failure(new RuntimeException())

    def "Should fetch through primary fetcher"() {
        given:
            primaryFetcher.fetch(URL) >> Try.success(syndFeed)

        when:
            def actual = feedProducer.createFor(URL)

        then:
            actual.isDefined()
            actual.get() == syndFeed
        and:
            0 * fallbackFetcher.fetch(URL)
            0 * lastFallbackFetcher.fetch(URL)
    }

    def "Should fallback to second fetcher"() {
        given:
            primaryFetcher.fetch(URL) >> failure
            fallbackFetcher.fetch(URL) >> Try.success(syndFeed)

        when:
            def actual = feedProducer.createFor(URL)

        then:
            actual.isDefined()
            actual.get() == syndFeed
        and:
            0 * lastFallbackFetcher.fetch(URL)
    }

    def "Should fallback to any fetcher"() {
        given:
            primaryFetcher.fetch(URL) >> failure
            fallbackFetcher.fetch(URL) >> failure
            lastFallbackFetcher.fetch(URL) >> Try.success(syndFeed)

        when:
            def actual = feedProducer.createFor(URL)

        then:
            actual.isDefined()
            actual.get() == syndFeed
    }

    def "Should return Option.None when all methods fail"() {
        given:
            primaryFetcher.fetch(URL) >> failure
            fallbackFetcher.fetch(URL) >> failure
            lastFallbackFetcher.fetch(URL) >> failure

        when:
            def actual = feedProducer.createFor(URL)

        then:
            actual.isEmpty()
    }
}
