package com.jvm_bloggers.core.rss.converters

import com.jvm_bloggers.utils.DateTimeUtilities
import com.jvm_bloggers.utils.ZoneTimeProvider
import com.rometools.rome.feed.synd.*
import groovy.json.JsonSlurper
import org.apache.groovy.json.internal.LazyMap
import spock.lang.Shared
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime

import static com.jvm_bloggers.utils.DateTimeUtilities.toDate

@Subject(SyndFeedToJsonConverter)
class SyndFeedToJsonConverterSpec extends Specification {

    static final String BASE_URL = "http://localhost"
    static final String FEED_LINK = "http://localhost/pl/rss.json"
    static final String FEED_TITLE = "JvmBloggers"
    static final LocalDateTime DATE = new ZoneTimeProvider().now()

    static final String AUTHOR_1 = "author1"
    static final String AUTHOR_2 = "author2"
    static final String DESCRIPTION_1 = "description1"
    static final String TITLE_1 = "title1"
    static final String TITLE_2 = "title2"

    SyndFeedToJsonConverter converter = new SyndFeedToJsonConverter(BASE_URL)

    SyndFeed feed = Mock() {
        SyndEntry entry1 = stubFeedEntry(AUTHOR_1, BASE_URL, DESCRIPTION_1, TITLE_1, DATE)
        SyndEntry entry2 = stubFeedEntry(AUTHOR_2, BASE_URL, null, TITLE_2, DATE)
        SyndLink link = new SyndLinkImpl()
        link.setRel("self")
        link.setHref(FEED_LINK)

        getTitle() >> FEED_TITLE
        getLinks() >> [link]
        getGenerator() >> BASE_URL

        getEntries() >> [entry1, entry2]
    }

    def "Should convert RSS feed to a JSON content"() {
        when:
        def jsonString = converter.convert(feed).toString()
        def json = new JsonSlurper().parseText(jsonString)

        then:
        json.with {
            title == this.FEED_TITLE
            link == this.FEED_LINK
            generator == this.BASE_URL

            entries != null
            entries.size() == 2
        }

        and:
        json.entries[0].with {
            author == this.AUTHOR_1
            link == this.BASE_URL
            description == this.DESCRIPTION_1
            title == this.TITLE_1
            date == DateTimeUtilities.DATE_TIME_FORMATTER.format(this.DATE)
        }

        and:
        json.entries[1].with {
            author == this.AUTHOR_2
            link == this.BASE_URL
            description == null
            title == this.TITLE_2
            date == DateTimeUtilities.DATE_TIME_FORMATTER.format(this.DATE)
        }
    }

    def stubFeedEntry(String author, String link, String description, String title, LocalDateTime publishedDate) {
        SyndContent content = Stub() {
            getValue() >> description
        }

        return Stub(SyndEntry) {
            getAuthor() >> author
            getDescription() >> content
            getTitle() >> title
            getLink() >> link
            getPublishedDate() >> toDate(publishedDate)
        }
    }
}