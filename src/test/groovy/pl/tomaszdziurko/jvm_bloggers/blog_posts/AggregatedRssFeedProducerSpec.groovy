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
    def TITLE = "title"
    def URL = "http://blogPostUrl"
    def AUTHOR = "author"
    def DATE = new NowProvider().now()
    def UID_1 = UUID.randomUUID().toString()
    def UID_2 = UUID.randomUUID().toString()
    def REQUEST_URL = "http://jvm-bloggers.com/rss"

    BlogPostRepository blogPostRepository = Mock()
    NowProvider nowProvider = Stub() {
        now() >> DATE
    }

    @Subject
    AggregatedRssFeedProducer rssProducer = new AggregatedRssFeedProducer(blogPostRepository, nowProvider)

    def "Should produce aggregated RSS feed"() {
        given:
            BlogPost blogPost1 = mockBlogPost(UID_1, DESCRIPTION, TITLE, URL, AUTHOR, DATE)
            BlogPost blogPost2 = mockBlogPost(UID_2, null, TITLE, URL, AUTHOR, DATE)
            1 * blogPostRepository.findByApprovedTrueOrderByPublishedDateDesc() >> [blogPost1, blogPost2]
        when:
            SyndFeed feed = rssProducer.getRss(REQUEST_URL)
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
                link == URL + EXPECTED_UTM_SUBSTRING
                title == TITLE
                author == AUTHOR
                description.value == DESCRIPTION
                publishedDate == date
                author == AUTHOR
            }
        and:
            with (feed.entries[1]) {
                link == URL + EXPECTED_UTM_SUBSTRING
                title == TITLE
                author == AUTHOR
                description == null
                publishedDate == date
                author == AUTHOR
            }

    }

    def mockBlogPost(String uid, String description, String title, String url, String author, LocalDateTime date) {
        BlogPost blogPost = Mock()
        1 * blogPost.getDescription() >> description
        1 * blogPost.getTitle() >> title
        1 * blogPost.getUrl() >> url
        1 * blogPost.getUid() >> uid 
        Blog blog = Mock()
        blog.getAuthor() >> author
        1 * blogPost.getBlog() >> blog
        1 * blogPost.getPublishedDate() >> date
        return blogPost
    }

}
