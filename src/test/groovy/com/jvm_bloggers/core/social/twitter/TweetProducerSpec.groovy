package com.jvm_bloggers.core.social.twitter

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator
import com.jvm_bloggers.core.newsletter_issues.NewIssuePublished
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue
import com.jvm_bloggers.entities.twitter.TweetRepository
import com.jvm_bloggers.utils.NowProvider
import com.jvm_bloggers.utils.ZoneTimeProvider
import spock.lang.Specification
import spock.lang.Subject

import static com.jvm_bloggers.entities.blog.BlogType.COMPANY
import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

@Subject(TweetProducer)
class TweetProducerSpec extends Specification {

    private static final long ISSUE_NUMBER = 999L
    private static final String LINK = "http://jvm-bloggers.com/issue/$ISSUE_NUMBER"

    private static final Random randomJsonId = new Random()
    private static final NowProvider nowProvider = new ZoneTimeProvider()

    private LinkGenerator linkGenerator = Stub(LinkGenerator)

    def setup() {
        linkGenerator.generateIssueLink(_) >> { args -> LINK }
    }

    def "Should save a new Facebook post for a given issue"() {
        given:
        TweetContentGenerator contentGenerator = new TweetContentGenerator(linkGenerator)
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