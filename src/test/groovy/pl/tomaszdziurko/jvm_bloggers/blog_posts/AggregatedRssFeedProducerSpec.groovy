package pl.tomaszdziurko.jvm_bloggers.blog_posts

import static pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities.toDate

import java.time.LocalDateTime

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider
import spock.lang.Specification
import spock.lang.Subject

import com.sun.syndication.feed.synd.SyndFeed

class AggregatedRssFeedProducerSpec extends Specification {

    def EXPECTED_UTM_SUBSTRING = "?utm_source=jvm-bloggers.com&utm_medium=RSS&utm_campaign=RSS@"
    def DESCRIPTION = "description"
    def TITLE = "title"
    def URL = "http://blogPostUrl"
    def AUTHOR = "author"
    def DATE = new NowProvider().now()

    BlogPostRepository blogPostRepository = Mock()
    NowProvider nowProvider = Stub() {
        now() >> DATE
    }

    @Subject
    AggregatedRssFeedProducer rssProducer = new AggregatedRssFeedProducer(blogPostRepository, nowProvider)

    def "Should produce aggregated RSS feed"() {
        given:
            def blogPost1 = mockBlogPost(DESCRIPTION, TITLE, URL, AUTHOR, DATE)
            def blogPost2 = mockBlogPost(null, TITLE, URL, AUTHOR, DATE)
            1 * blogPostRepository.findByApprovedTrueOrderByPublishedDateDesc() >> [blogPost1, blogPost2]
        when:
            SyndFeed feed = rssProducer.getRss()
        then:
            def date = toDate(DATE)
            def campaignDate = date.toString().replace(" ", "-")
            feed.feedType == AggregatedRssFeedProducer.FEED_TYPE
            feed.title == AggregatedRssFeedProducer.FEED_TITLE
            feed.title == AggregatedRssFeedProducer.FEED_TITLE
            feed.description == AggregatedRssFeedProducer.FEED_DESCRIPTION
            feed.publishedDate == date
            feed.entries.size() == 2
        and:
            with (feed.entries[0]) {
                link == URL + EXPECTED_UTM_SUBSTRING + campaignDate
                title == TITLE
                author == AUTHOR
                description.value == DESCRIPTION
                publishedDate == date
                author == AUTHOR
            }
        and:
            with (feed.entries[1]) {
                link == URL + EXPECTED_UTM_SUBSTRING + campaignDate
                title == TITLE
                author == AUTHOR
                description == null
                publishedDate == date
                author == AUTHOR
            }

    }

    def mockBlogPost(String description, String title, String url, String author, LocalDateTime date) {
        BlogPost blogPost = Mock()
        1 * blogPost.getDescription() >> description
        1 * blogPost.getTitle() >> title
        1 * blogPost.getUrl() >> url
        Blog blog = Mock()
        blog.getAuthor() >> author
        1 * blogPost.getBlog() >> blog
        1 * blogPost.getPublishedDate() >> date
        return blogPost
    }

}
