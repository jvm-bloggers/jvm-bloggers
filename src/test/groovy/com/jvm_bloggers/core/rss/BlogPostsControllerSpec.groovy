package com.jvm_bloggers.core.rss

import com.jvm_bloggers.core.rss.json.SyndFeedToJsonConverter
import com.rometools.rome.feed.synd.*
import org.apache.commons.io.IOUtils
import org.springframework.test.web.servlet.MockMvc
import spock.lang.Shared
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import java.nio.charset.Charset

import static org.springframework.http.MediaType.APPLICATION_ATOM_XML_VALUE
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup

class BlogPostsControllerSpec extends Specification {

    @Shared
    Date today = new Date(0)

    AggregatedRssFeedProducer rssProducerMock = Mock() {
        SyndLink link = new SyndLinkImpl()
        link.rel = "self"
        link.href = "http://jvm-bloggers.com/rss"

        SyndFeed feed = new SyndFeedImpl()
        feed.links = [link]
        feed.uri = "URI"
        feed.feedType = AggregatedRssFeedProducer.FEED_TYPE
        feed.title = AggregatedRssFeedProducer.FEED_TITLE
        feed.description = AggregatedRssFeedProducer.FEED_DESCRIPTION
        feed.publishedDate = today
        feed.entries = [createSyndEntry("postId", "postUrl", "postTitle", "postAuthor", "postDescription", today)]

        1 * getRss(_, _, _) >> feed
    }

    SyndFeedToJsonConverter syndFeedToJsonConverter = new SyndFeedToJsonConverter("http://jvm-bloggers.com")

    @Subject
    BlogPostsController blogPostsController = new BlogPostsController(rssProducerMock, syndFeedToJsonConverter, 50)

    @Unroll
    def "Should get OK status for RSS feed in #format format request"() {
        given:
        MockMvc mockMvc = standaloneSetup(blogPostsController).build()

        expect:
        mockMvc.perform(get("/pl/rss?format=$format"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(mediaType))

        where:
        format || mediaType
        "json" || APPLICATION_JSON_VALUE
        "xml"  || APPLICATION_ATOM_XML_VALUE
        ""     || APPLICATION_ATOM_XML_VALUE
    }

    def "Should get BAD_REQUEST status for RSS feed in undefined format request"() {
        given:
        MockMvc mockMvc = standaloneSetup(blogPostsController).build()

        expect:
        mockMvc.perform(get("/pl/rss?format=dummy_format"))
                .andExpect(status().isBadRequest())
    }

    @Unroll
    def "Should get RSS feed as #format"() {
        given:
        HttpServletRequest request = Stub() {
            getRequestURL() >> new StringBuffer("http://jvm-bloggers.com/rss")
        }

        HttpServletResponse response = Mock()
        StringWriter actualOutput = new StringWriter()
        PrintWriter printWriter = new PrintWriter(actualOutput)

        when:
        blogPostsController.getRss(request, response, printWriter, null, null, format)

        then:
        1 * response.setContentType(mediaType)

        def actualLines = IOUtils.readLines(IOUtils.toInputStream(actualOutput.toString(), Charset.defaultCharset()), Charset.defaultCharset())
        def expectedLines = IOUtils.readLines(getClass().getResource(resource).openStream(), Charset.defaultCharset())

        actualLines == expectedLines

        where:
        format || resource            || mediaType
        "xml"  || "expected-rss.xml"  || APPLICATION_ATOM_XML_VALUE
        "json" || "expected-rss.json" || APPLICATION_JSON_VALUE
    }

    def createSyndEntry(String id, String url, String title, String author, String description, Date publishedDate) {
        SyndContent descriptionContent = new SyndContentImpl()
        descriptionContent.value = description

        SyndEntry entry = new SyndEntryImpl()
        entry.uri = id
        entry.link = url
        entry.title = title
        entry.author = author
        entry.description = descriptionContent
        entry.publishedDate = publishedDate

        return entry
    }
}
