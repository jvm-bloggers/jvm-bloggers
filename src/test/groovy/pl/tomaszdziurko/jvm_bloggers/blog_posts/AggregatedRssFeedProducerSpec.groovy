package pl.tomaszdziurko.jvm_bloggers.blog_posts

import com.rometools.rome.feed.synd.SyndFeed
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime

import javax.servlet.http.HttpServletRequest;

import static pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities.toDate

class AggregatedRssFeedProducerSpec extends Specification {

    def EXPECTED_UTM_SUBSTRING = "?utm_source=jvm-bloggers.com&utm_medium=RSS&utm_campaign=RSS"
    def DESCRIPTION = "description"
    def TITLE_1 = "title_1"
    def TITLE_2 = "title_2"
    def URL_1 = "http://blogPostUrl_1"
    def URL_2 = "http://blogPostUrl_2"
    def AUTHOR_1 = "author_1"
    def AUTHOR_2 = "author_2"
    def DATE = new NowProvider().now()
    def UID_1 = UUID.randomUUID().toString()
    def UID_2 = UUID.randomUUID().toString()
    def REQUEST_URL = "http://jvm-bloggers.com/rss"

    BlogPostRepository blogPostRepository = Stub() {
        BlogPost blogPost1 = stubBlogPost(UID_1, DESCRIPTION, TITLE_1, URL_1, AUTHOR_1, DATE)
        BlogPost blogPost2 = stubBlogPost(UID_2, null, TITLE_2, URL_2, AUTHOR_2, DATE)
        findByApprovedTrueOrderByPublishedDateDesc() >> [blogPost1, blogPost2]
    }
    
    NowProvider nowProvider = Stub() {
        now() >> DATE
    }

    @Subject
    AggregatedRssFeedProducer rssProducer = new AggregatedRssFeedProducer(blogPostRepository, nowProvider)

    def "Should produce aggregated RSS feed with all entries"() {
        when:
            SyndFeed feed = rssProducer.getRss(REQUEST_URL, 0)
        then:
            Date date = toDate(DATE)
            with (feed) {
                links[0].rel == "self"
                links[0].href == REQUEST_URL
                feedType == AggregatedRssFeedProducer.FEED_TYPE
                uri == AggregatedRssFeedProducer.FEED_TITLE
                title == AggregatedRssFeedProducer.FEED_TITLE
                description == AggregatedRssFeedProducer.FEED_DESCRIPTION
                publishedDate == date
                entries.size() == 2
            }
        and:
            with (feed.entries[0]) {
                link == URL_1 + EXPECTED_UTM_SUBSTRING
                title == TITLE_1
                author == AUTHOR_1
                description.value == DESCRIPTION
                publishedDate == date
                author == AUTHOR_1
                uri == UID_1
            }
        and:
            with (feed.entries[1]) {
                link == URL_2 + EXPECTED_UTM_SUBSTRING
                title == TITLE_2
                author == AUTHOR_2
                description == null
                publishedDate == date
                author == AUTHOR_2
                uri == UID_2
            }

    }

    def "Should produce aggregated RSS feed with limited entries count"() {
        when:
            SyndFeed feed = rssProducer.getRss(REQUEST_URL, 1)
        then:
            Date date = toDate(DATE)
            with (feed) {
                links[0].rel == "self"
                links[0].href == REQUEST_URL
                feedType == AggregatedRssFeedProducer.FEED_TYPE
                uri == AggregatedRssFeedProducer.FEED_TITLE
                title == AggregatedRssFeedProducer.FEED_TITLE
                description == AggregatedRssFeedProducer.FEED_DESCRIPTION
                publishedDate == date
                entries.size() == 1
            }
        and:
            with (feed.entries[0]) {
                link == URL_1 + EXPECTED_UTM_SUBSTRING
                title == TITLE_1
                author == AUTHOR_1
                description.value == DESCRIPTION
                publishedDate == date
                author == AUTHOR_1
            }

    }

    def "Should throw IAE on blank feedUrl parameter"() {
        when:
            rssProducer.getRss(blankFeedUrl, 1)
        then:
            IllegalArgumentException e = thrown()
            e.getMessage() == "feedUrl parameter cannot be blank"
        where:
            blankFeedUrl << [" ", "", null]
    }

    def stubBlogPost(String uid, String description, String title, String url, String author, LocalDateTime date) {
        Blog blog = Stub() {
            getAuthor() >> author
        }
        return Stub(BlogPost) {
            getDescription() >> description
            getTitle() >> title
            getUrl() >> url
            getUid() >> uid 
            getBlog() >> blog
            getPublishedDate() >> date
        }
    }

}
