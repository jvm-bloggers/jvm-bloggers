package pl.tomaszdziurko.jvm_bloggers.blog_posts

import javax.servlet.http.HttpServletResponse

import org.apache.commons.io.IOUtils
import org.mockito.internal.util.io.IOUtil
import org.springframework.http.MediaType

import spock.lang.Specification
import spock.lang.Subject

import com.sun.syndication.feed.synd.SyndContentImpl
import com.sun.syndication.feed.synd.SyndEntry
import com.sun.syndication.feed.synd.SyndEntryImpl
import com.sun.syndication.feed.synd.SyndFeedImpl

class BlogPostsControllerSpec extends Specification {

    def today = new Date(0)

    AggregatedRssFeedProducer rssProducer = Mock() {
        SyndFeedImpl feed = new SyndFeedImpl()
        feed.feedType = AggregatedRssFeedProducer.FEED_TYPE
        feed.title = AggregatedRssFeedProducer.FEED_TITLE
        feed.description = AggregatedRssFeedProducer.FEED_DESCRIPTION
        feed.publishedDate = today
        feed.entries = [createSyndEntry("postUrl", "postTitle",  "postAuthor", "postDescription", today)]
        1 * getRss() >> feed
    }

    @Subject
    BlogPostsController blogPostsController = new BlogPostsController(rssProducer)

    def "Should get RSS feed"() {
        given:
            HttpServletResponse response = Mock()
            def actualOutput = new ByteArrayOutputStream()
        when:
            blogPostsController.getRss(response, new PrintWriter(actualOutput))
        then:
            1 * response.setContentType(MediaType.APPLICATION_ATOM_XML_VALUE)
            def actualLines = IOUtils.readLines(IOUtils.toInputStream(actualOutput.toString()))
            def expectedLines = IOUtils.readLines(getClass().getResource("expected-rss.xml").openStream())
            actualLines == expectedLines
    }

    protected createSyndEntry(String url, String title, String author, String description, Date publishedDate) {
        def entry = new SyndEntryImpl()
        entry.link = url
        entry.title = title
        entry.author = author
        def descriptionContent = new SyndContentImpl()
        descriptionContent.value = description
        entry.description = descriptionContent
        entry.publishedDate = publishedDate
        return entry
    }
}
