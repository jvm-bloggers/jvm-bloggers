package pl.tomaszdziurko.jvm_bloggers.blog_posts

import com.rometools.rome.feed.synd.SyndFeed
import org.springframework.data.domain.Pageable
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalDateTime

import javax.servlet.http.HttpServletRequest;

import static pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities.toDate

class AggregatedRssFeedProducerSpec extends Specification {

    String EXPECTED_UTM_SUBSTRING = "?utm_source=jvm-bloggers.com&utm_medium=RSS&utm_campaign=RSS"
    String DESCRIPTION = "description"
    String TITLE_1 = "title_1"
    String TITLE_2 = "title_2"
    String URL_1 = "http://blogPostUrl-1.com"
    String URL_2 = "http://blogPostUrl-2.com"
    String INVALID_URL = "http://invalid-url"
    String AUTHOR_1 = "author_1"
    String AUTHOR_2 = "author_2"
    LocalDateTime DATE = new NowProvider().now()
    String UID_1 = UUID.randomUUID().toString()
    String UID_2 = UUID.randomUUID().toString()
    String UID_3 = UUID.randomUUID().toString()
    String REQUEST_URL = "http://jvm-bloggers.com/rss"

    BlogPostRepository blogPostRepository = Stub() {
        BlogPost blogPost1 = stubBlogPost(UID_1, DESCRIPTION, TITLE_1, URL_1, AUTHOR_1, DATE)
        BlogPost blogPost2 = stubBlogPost(UID_2, null, TITLE_2, URL_2, AUTHOR_2, DATE)
        BlogPost blogPost3 = stubBlogPost(UID_3, null, TITLE_2, INVALID_URL, AUTHOR_2, DATE)
        findByApprovedTrueOrderByPublishedDateDesc(_) >> { args ->
            Pageable pageable = args[0]
            List<BlogPost> blogposts = [blogPost1, blogPost2, blogPost3]
            int limit = Math.min(blogposts.size(), pageable.pageSize)
            return blogposts.subList(0, limit)
        }
    }

    NowProvider nowProvider = Stub() {
        now() >> DATE
    }

    @Subject
    AggregatedRssFeedProducer rssProducer = new AggregatedRssFeedProducer(blogPostRepository, nowProvider)

    def "Should produce aggregated RSS feed with all entries having valid url"() {
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

    @Unroll
    def "Should throw IAE on invalid feedUrl = [#blankFeedUrl]"() {
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
