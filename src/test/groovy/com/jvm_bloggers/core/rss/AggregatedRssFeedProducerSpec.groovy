package com.jvm_bloggers.core.rss

import com.jvm_bloggers.TestTimeProvider
import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator
import com.jvm_bloggers.core.blogpost_redirect.RedirectController
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.utils.NowProvider
import com.jvm_bloggers.utils.ZoneTimeProvider
import com.rometools.rome.feed.synd.SyndFeed
import org.springframework.data.domain.Pageable
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalDateTime

import static com.jvm_bloggers.core.rss.AggregatedRssFeedProducer.INCLUDE_ALL_AUTHORS_SET
import static com.jvm_bloggers.utils.DateTimeUtilities.toDate

@Subject(AggregatedRssFeedProducer)
class AggregatedRssFeedProducerSpec extends Specification {

    public static final String BASE_URL = 'http://test'
    public static final String ISSUE_URL = 'http://test/issue'
    public static final String TOP_POSTS_URL = 'http://test/topPosts'
    String TITLE_1 = 'title_1', TITLE_2 = 'title_2'
    String URL_1 = 'http://blogPostUrl-1.com', URL_2 = 'http://blogPostUrl-2.com'
    String AUTHOR_1 = 'author_1', AUTHOR_2 = 'author_2'
    String DESCRIPTION = 'description'
    String INVALID_URL = 'http://invalid-url'
    String REQUEST_URL = 'http://jvm-bloggers.com/rss'
    LocalDateTime DATE = new ZoneTimeProvider().now()
    String UID_1 = UUID.randomUUID().toString()
    String UID_2 = UUID.randomUUID().toString()
    String UID_3 = UUID.randomUUID().toString()

    BlogPostRepository blogPostRepository = Mock() {
        BlogPost blogPost1 = stubBlogPost(UID_1, DESCRIPTION, TITLE_1, URL_1, AUTHOR_1, DATE)
        BlogPost blogPost2 = stubBlogPost(UID_2, null, TITLE_2, URL_2, AUTHOR_2, DATE)
        BlogPost blogPost3 = stubBlogPost(UID_3, null, TITLE_2, INVALID_URL, AUTHOR_2, DATE)
        findByApprovedTrueAndBlogAuthorNotInOrderByApprovedDateDesc(_, _) >> { args ->
            Pageable pageable = args[0]
            List<BlogPost> blogposts = [blogPost1, blogPost2, blogPost3]
            int limit = Math.min(blogposts.size(), pageable.pageSize)
            return blogposts.subList(0, limit)
        }
    }

    NowProvider nowProvider = new TestTimeProvider(DATE);

    LinkGenerator linkGenerator = new LinkGenerator(BASE_URL, ISSUE_URL, TOP_POSTS_URL)

    AggregatedRssFeedProducer rssProducer = new AggregatedRssFeedProducer(blogPostRepository, nowProvider, linkGenerator)

    def "Should produce aggregated RSS feed with all entries having valid url"() {
        when:
        SyndFeed feed = rssProducer.getRss(REQUEST_URL, 0, null)

        then:
        Date date = toDate(DATE)
        with(feed) {
            links[0].rel == 'self'
            links[0].href == REQUEST_URL
            feedType == AggregatedRssFeedProducer.FEED_TYPE
            uri == AggregatedRssFeedProducer.FEED_TITLE
            title == AggregatedRssFeedProducer.FEED_TITLE
            description == AggregatedRssFeedProducer.FEED_DESCRIPTION
            publishedDate == date
            entries.size() == 2
        }

        and:
        with(feed.entries[0]) {
            link == redirectUrlForUid(UID_1)
            title == TITLE_1
            author == AUTHOR_1
            description.value == DESCRIPTION
            publishedDate == date
            author == AUTHOR_1
            uri == UID_1
        }

        and:
        with(feed.entries[1]) {
            link == redirectUrlForUid(UID_2)
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
        SyndFeed feed = rssProducer.getRss(REQUEST_URL, 1, null)

        then:
        Date date = toDate(DATE)
        with(feed) {
            links[0].rel == 'self'
            links[0].href == REQUEST_URL
            feedType == AggregatedRssFeedProducer.FEED_TYPE
            uri == AggregatedRssFeedProducer.FEED_TITLE
            title == AggregatedRssFeedProducer.FEED_TITLE
            description == AggregatedRssFeedProducer.FEED_DESCRIPTION
            publishedDate == date
            entries.size() == 1
        }

        and:
        with(feed.entries[0]) {
            link == redirectUrlForUid(UID_1)
            title == TITLE_1
            author == AUTHOR_1
            description.value == DESCRIPTION
            publishedDate == date
            author == AUTHOR_1
            uri == UID_1
        }
    }

    private String redirectUrlForUid(String uid) {
        return BASE_URL + RedirectController.REDIRECT_URL_PATH + '/' + uid
    }

    @Unroll
    def "Should throw IAE on invalid feedUrl = [#blankFeedUrl]"() {
        when:
        rssProducer.getRss(blankFeedUrl, 1, null)

        then:
        IllegalArgumentException e = thrown()
        e.getMessage() == 'feedUrl parameter cannot be blank'

        where:
        blankFeedUrl << [' ', '', null]
    }

    def "Should filter out given authors"() {
        given:
        Set excludedAuthors = ["John Doe", "Jane Doe"]

        when:
        rssProducer.getRss(REQUEST_URL, 0, excludedAuthors)

        then:
        1 * blogPostRepository.findByApprovedTrueAndBlogAuthorNotInOrderByApprovedDateDesc(_, excludedAuthors) >> []
    }

    @Unroll
    def "Should include all authors when 'excludedAuthors' is #excludedAuthors"() {
        when:
        rssProducer.getRss(REQUEST_URL, 0, excludedAuthors)

        then:
        1 * blogPostRepository.findByApprovedTrueAndBlogAuthorNotInOrderByApprovedDateDesc(_, INCLUDE_ALL_AUTHORS_SET) >> []

        where:
        excludedAuthors << [null, [] as Set]
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
