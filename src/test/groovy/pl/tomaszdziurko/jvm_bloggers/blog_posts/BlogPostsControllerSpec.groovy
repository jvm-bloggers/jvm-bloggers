package pl.tomaszdziurko.jvm_bloggers.blog_posts

import com.rometools.rome.feed.synd.SyndContentImpl
import com.rometools.rome.feed.synd.SyndEntryImpl
import com.rometools.rome.feed.synd.SyndFeedImpl
import com.rometools.rome.feed.synd.SyndLinkImpl;

import org.apache.commons.io.IOUtils
import org.springframework.http.MediaType
import spock.lang.Specification
import spock.lang.Subject

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse

class BlogPostsControllerSpec extends Specification {

    def today = new Date(0)

    AggregatedRssFeedProducer rssProducer = Mock() {
        def link = new SyndLinkImpl()
        link.rel = "self"
        link.href = "http://jvm-bloggers.com/rss"
        def feed = new SyndFeedImpl()
        feed.links = [link]
        feed.uri = "URI" 
        feed.feedType = AggregatedRssFeedProducer.FEED_TYPE
        feed.title = AggregatedRssFeedProducer.FEED_TITLE
        feed.description = AggregatedRssFeedProducer.FEED_DESCRIPTION
        feed.publishedDate = today
        feed.entries = [createSyndEntry("postId", "postUrl", "postTitle",  "postAuthor", "postDescription", today)]
        1 * getRss(_, _, _) >> feed
    }

    @Subject
    BlogPostsController blogPostsController = new BlogPostsController(rssProducer)

    def "Should get RSS feed"() {
        given:
            HttpServletRequest request = Stub() {
                getRequestURL() >> new StringBuffer("http://jvm-bloggers.com/rss")
            }
            HttpServletResponse response = Mock()
            def actualOutput = new ByteArrayOutputStream()
        when:
            blogPostsController.getRss(request, response, new PrintWriter(actualOutput), null, 0, null)
        then:
            1 * response.setContentType(MediaType.APPLICATION_ATOM_XML_VALUE)
            def actualLines = IOUtils.readLines(IOUtils.toInputStream(actualOutput.toString()))
            def expectedLines = IOUtils.readLines(getClass().getResource("expected-rss.xml").openStream())
            actualLines == expectedLines
    }

    protected createSyndEntry(String id, String url, String title, String author, String description, Date publishedDate) {
        def entry = new SyndEntryImpl()
        entry.uri = id
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
