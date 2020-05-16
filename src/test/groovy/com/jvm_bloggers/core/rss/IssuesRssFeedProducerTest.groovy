package com.jvm_bloggers.core.rss

import com.jvm_bloggers.TestTimeProvider
import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository
import com.jvm_bloggers.utils.NowProvider
import com.rometools.rome.feed.synd.SyndFeed
import io.vavr.collection.List
import io.vavr.collection.Seq
import org.springframework.data.domain.Pageable
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.Month

import static com.jvm_bloggers.utils.DateTimeUtilities.toDate

class IssuesRssFeedProducerTest extends Specification {

    private static final String BASE_URL = "http://test"
    private static final String ISSUE_URL = "http://test/issue/"
    private static final String APP_NAME = "JVM Bloggers"
    private static final String REQUEST_URL = "http://jvm-bloggers.com/feed/issues"
    private static final LocalDateTime DATE = LocalDateTime.of(2018, Month.JULY, 2, 20, 30)
    private static final Date EXPECTED_DATE = toDate(DATE)
    private static final String EXPECTED_TITLE_1 = "Wydanie #7"
    private static final String EXPECTED_TITLE_2 = "Wydanie #8"
    private static final String EXPECTED_TITLE_3 = "Wydanie #9"
    private static final String EXPECTED_URL_1 = "http://test/issue/7"
    private static final String EXPECTED_URL_2 = "http://test/issue/8"
    private static final String EXPECTED_URL_3 = "http://test/issue/9"
    private static final String EXPECTED_ID_1 = "7"
    private static final String EXPECTED_ID_2 = "8"
    private static final String EXPECTED_ID_3 = "9"

    private static final LocalDate PUBLISHED1 = LocalDate.of(2018, Month.JUNE, 10)
    private static final LocalDate PUBLISHED2 = LocalDate.of(2018, Month.JUNE, 20)
    private static final LocalDate PUBLISHED3 = LocalDate.of(2018, Month.JUNE, 30)

    NewsletterIssueRepository issueRepository = Stub() {
        NewsletterIssue issue1 = new NewsletterIssue(1L, 7L, PUBLISHED1, "", [], [], "")
        NewsletterIssue issue2 = new NewsletterIssue(2L, 8L, PUBLISHED2, "", [], [], "")
        NewsletterIssue issue3 = new NewsletterIssue(3L, 9L, PUBLISHED3, "", [], [], "")

        findByOrderByPublishedDateDesc(_) >> { args ->
            Pageable pageable = args[0] as Pageable
            Seq<NewsletterIssue> issues = List.of(issue3, issue2, issue1)
            int limit = Math.min(issues.size(), pageable.pageSize)
            return issues.slice(0, limit)
        }
    }

    LinkGenerator linkGenerator = new LinkGenerator(BASE_URL, ISSUE_URL, "")
    NowProvider nowProvider = new TestTimeProvider(DATE)

    @Subject
    IssuesRssFeedProducer testObj = new IssuesRssFeedProducer(issueRepository, linkGenerator, nowProvider, APP_NAME)

    def "Should produce issues RSS feed"() {
        when:
            SyndFeed feed = testObj.getRss(REQUEST_URL, 50)

        then:
        with(feed) {
            links[0].rel == "self"
            links[0].href == REQUEST_URL
            feedType == IssuesRssFeedProducer.FEED_TYPE
            uri == IssuesRssFeedProducer.FEED_TITLE
            title == IssuesRssFeedProducer.FEED_TITLE
            description == IssuesRssFeedProducer.FEED_DESCRIPTION
            publishedDate == EXPECTED_DATE
            entries.size() == 3
        }

        and:
        with(feed.entries[0]) {
            title == EXPECTED_TITLE_3
            link == EXPECTED_URL_3
            author == APP_NAME
            publishedDate == toDate(PUBLISHED3)
            uri == EXPECTED_ID_3
        }

        and:
        with(feed.entries[1]) {
            title == EXPECTED_TITLE_2
            link == EXPECTED_URL_2
            author == APP_NAME
            publishedDate == toDate(PUBLISHED2)
            uri == EXPECTED_ID_2
        }

        and:
        with(feed.entries[2]) {
            title == EXPECTED_TITLE_1
            link == EXPECTED_URL_1
            author == APP_NAME
            publishedDate == toDate(PUBLISHED1)
            uri == EXPECTED_ID_1
        }
    }

    def "Should limit the number of generated issues"() {
        when:
            SyndFeed feed = testObj.getRss(REQUEST_URL, 1)

        then:
        with(feed) {
            links[0].rel == "self"
            links[0].href == REQUEST_URL
            feedType == IssuesRssFeedProducer.FEED_TYPE
            uri == IssuesRssFeedProducer.FEED_TITLE
            title == IssuesRssFeedProducer.FEED_TITLE
            description == IssuesRssFeedProducer.FEED_DESCRIPTION
            publishedDate == EXPECTED_DATE
            entries.size() == 1
        }

        and:
        with(feed.entries[0]) {
            title == EXPECTED_TITLE_3
            link == EXPECTED_URL_3
            author == APP_NAME
            publishedDate == toDate(PUBLISHED3)
            uri == "9"
        }
    }

    @Unroll
    def "Should throw IAE on invalid feedUrl = [#blankFeedUrl]"() {
        when:
        testObj.getRss(blankFeedUrl, 1)

        then:
        IllegalArgumentException e = thrown()
        e.getMessage() == "feedUrl parameter cannot be blank"

        where:
        blankFeedUrl << [" ", "", null]
    }
}
