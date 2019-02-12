package com.jvm_bloggers.core.rss.converters

import com.jvm_bloggers.core.rss.TestSyndFeedProvider
import com.rometools.rome.feed.synd.SyndFeed
import com.rometools.rome.feed.synd.SyndFeedImpl
import org.apache.commons.io.IOUtils
import org.springframework.http.HttpInputMessage
import org.springframework.http.HttpOutputMessage
import org.springframework.mock.http.MockHttpOutputMessage
import spock.lang.Shared
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import java.nio.charset.Charset

import static org.hamcrest.Matchers.equalToIgnoringWhiteSpace
import static spock.util.matcher.HamcrestSupport.that

@Subject(SyndFeedXmlMessageConverter)
class SyndFeedXmlMessageConverterSpec extends Specification {

    @Shared
    TestSyndFeedProvider testSyndFeedProvider = new TestSyndFeedProvider()

    SyndFeedXmlMessageConverter converter = new SyndFeedXmlMessageConverter()

    @Unroll
    def "Should support #supportedClazz"() {
        when:
        def result = converter.supports(supportedClazz)

        then:
        result == true

        where:
        supportedClazz << [SyndFeed, SyndFeedImpl]
    }

    def "Should throw an exception when trying create a SyndFeed object"() {
        when:
        converter.readInternal(SyndFeed, Mock(HttpInputMessage))

        then:
        thrown(UnsupportedOperationException)
    }

    def "Should properly convert SyndFeed object to the XML form"() {
        given:
        SyndFeed feed = testSyndFeedProvider.getSyndFeed()
        HttpOutputMessage message = new MockHttpOutputMessage()

        when:
        converter.writeInternal(feed, message)

        then:
        def actual = message.getBodyAsString(Charset.defaultCharset())
        def expected = IOUtils.toString(getClass().getResource("expected-rss.xml").openStream(), Charset.defaultCharset())

        that actual, equalToIgnoringWhiteSpace(expected)
    }
}
