package com.jvm_bloggers.core.social.twitter

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator
import com.jvm_bloggers.core.data_fetching.blogs.NewBlogAdded
import com.jvm_bloggers.core.newsletter_issues.NewIssuePublished
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository
import com.jvm_bloggers.entities.twitter.Tweet
import com.jvm_bloggers.entities.twitter.TweetRepository
import com.jvm_bloggers.utils.NowProvider
import com.jvm_bloggers.utils.TimeRange
import io.vavr.control.Option
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime
import java.time.LocalTime
import java.time.Month

import static com.jvm_bloggers.entities.blog.BlogType.COMPANY
import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

@Subject(TweetProducer)
class TweetProducerSpec extends Specification {

    private static final long ISSUE_NUMBER = 999L
    private static final String LINK = "http://jvm-bloggers.com/issue/$ISSUE_NUMBER"
    private static final LocalDateTime NOW = LocalDateTime.of(2020, Month.JANUARY, 1, 18, 0)

    private static final Random randomJsonId = new Random()

    private LinkGenerator linkGenerator = Stub(LinkGenerator)
    private NewsletterIssueRepository newsletterIssueRepository = Stub()
    private NowProvider nowProvider = Mock()

    def setup() {
        linkGenerator.generateIssueLink(_) >> { args -> LINK }
        nowProvider.now() >> NOW
    }

    def "Should save a new Facebook post for a given issue"() {
        given:
        TweetContentGenerator contentGenerator = new TweetContentGenerator(linkGenerator, newsletterIssueRepository)
        TweetRepository tweetRepository = Mock(TweetRepository)

        and:
        NewIssuePublished issuePublishedEvent = new NewIssuePublished(
                NewsletterIssue
                        .builder()
                        .issueNumber(ISSUE_NUMBER)
                        .heading("issue heading")
                        .blogPosts(posts())
                        .build()
        )
        and:
        TweetProducer tweetProducer = new TweetProducer(contentGenerator, tweetRepository, nowProvider)

        when:
        tweetProducer.handleNewIssueEvent(issuePublishedEvent)

        then:
        1 * tweetRepository.save({ it.content.contains(LINK) })
    }

    def "Should save two tweets for a new blog, one for now, and one for the future"() {
        given:
        TweetContentGenerator contentGenerator = new TweetContentGenerator(linkGenerator, newsletterIssueRepository)
        TweetRepository tweetRepository = Mock()
        TweetProducer tweetProducer = new TweetProducer(contentGenerator, tweetRepository, nowProvider)

        and:
        NewBlogAdded newBlogAdded = new NewBlogAdded(blog("@foobar", PERSONAL))

        and:
        TimeRange allowedPublicationTimeRange = TimeRange.between(LocalTime.of(8, 0), LocalTime.of(23, 59))

        and:
        newsletterIssueRepository.findFirstByOrderByPublishedDateDesc() >> Option.of(
                NewsletterIssue.builder().issueNumber(ISSUE_NUMBER).build())

        when:
        tweetProducer.handleNewBlogAddedEvent(newBlogAdded)

        then:
        1 * tweetRepository.saveAll({ Iterable<Tweet> tweets ->
            tweets.size() == 2

            LocalDateTime firstTweetPostingDate = tweets.first().postingDate
            LocalDateTime secondTweetPostingDate = tweets.last().postingDate
            firstTweetPostingDate.isBefore(secondTweetPostingDate)

            allowedPublicationTimeRange.contains(firstTweetPostingDate.toLocalTime())
            allowedPublicationTimeRange.contains(secondTweetPostingDate.toLocalTime())

            tweets.first().content != tweets.last().content
        })
    }

    private Collection<BlogPost> posts() {
        List<BlogPost> posts = new ArrayList<>()
        posts.add(blogPost(blog("@personal1", PERSONAL)))
        posts.add(blogPost(blog("@personal2", PERSONAL)))
        posts.add(blogPost(blog("@company1", COMPANY)))
        return posts
    }

    private BlogPost blogPost(Blog blog) {
        BlogPost
                .builder()
                .title("title")
                .url("url")
                .publishedDate(nowProvider.now())
                .blog(blog)
                .build()
    }

    private Blog blog(String twitterHandle, BlogType blogType) {
        Blog.builder()
                .bookmarkableId(randomJsonId.nextLong().toString())
                .author("author")
                .twitter(twitterHandle)
                .rss("rss")
                .url("url")
                .dateAdded(nowProvider.now())
                .blogType(blogType)
                .build()
    }

}
