package com.jvm_bloggers.core.social.twitter

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue
import com.jvm_bloggers.utils.NowProvider
import spock.lang.Specification

import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

class TweetContentGeneratorSpec extends Specification {

    private static final long ISSUE_NUMBER = 999L
    private static final String LINK = "http://jvm-bloggers.com/issue/$ISSUE_NUMBER"

    private final Random randomJsonId = new Random()
    private final NowProvider nowProvider = new NowProvider()

    def "Should generate a Tweet content with an issue number and link"() {
        given:
            LinkGenerator linkGenerator = Mock(LinkGenerator)
            linkGenerator.generateIssueLink(_) >> { args -> LINK }
            TweetContentGenerator generator = new TweetContentGenerator(linkGenerator)
        and:
            NewsletterIssue issue = NewsletterIssue
                    .builder()
                    .issueNumber(ISSUE_NUMBER)
                    .heading("issue heading")
                    .blogPosts(posts())
                    .build()

        when:
            String tweetContent = generator.generateTweetContent(issue)

        then:
            tweetContent.contains(issue.issueNumber as String)
            tweetContent.contains(LINK)
    }

    // Should add two twitter handles of personal blogs
    // Should add one twitter handle of company blog
    // Should add company twitter handle as the second on handles list
    // Should not add the second personal twitter handle if message is too long
    // Should always have java and jvm tags at the end


    private Collection<BlogPost> posts() {
        List<BlogPost> posts = new ArrayList<>()
        posts.add(blogPost(blog("author1")))
        posts.add(blogPost(blog("author2")))
        posts.add(blogPost(blog("author3")))
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

    private Blog blog(String twitterHandle) {
        Blog.builder()
            .jsonId(randomJsonId.nextLong())
            .author("author")
            .twitter(twitterHandle)
            .rss("rss")
            .url("url")
            .dateAdded(nowProvider.now())
            .blogType(PERSONAL)
            .build()
    }

}
