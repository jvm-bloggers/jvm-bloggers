package com.jvm_bloggers.frontend.newsletter_issue

import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPost
import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogType
import com.jvm_bloggers.core.blogpost_redirect.RedirectLinkGenerator
import com.jvm_bloggers.core.newsletter_issues.domain.NewsletterIssue
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDate

import static java.time.LocalDateTime.now

class NewsletterIssueDtoBuilderSpec extends Specification {

    static final String SHORT_URL = "http://shortlink.com"

    RedirectLinkGenerator generator = Stub(RedirectLinkGenerator) {
        generateLinkFor(_ as String ) >> SHORT_URL
    }

    @Subject
    NewsletterIssueDtoBuilder builder = new NewsletterIssueDtoBuilder(generator)

    def "Should convert newsletter issue to its DTO representation"() {
        given:
            Blog blog = new Blog(1, 2, "some author", "some rss", "some url",
                    "some twitter", now(), BlogType.PERSONAL, true)
            BlogPost post = new BlogPost(1, "some title", "some description", "some url", now(), true, blog)
            NewsletterIssue issue = new NewsletterIssue(1, 2, LocalDate.now(), "Some heading", [post],
                    [blog], "Some varia")
        when:
            NewsletterIssueDto issueDto = builder.build(issue)
        then:
            issueDto.heading == issue.getHeading()
            issueDto.varia == issue.getVaria()
            issueDto.number == issue.getIssueNumber()
            issueDto.publishedDate == issue.getPublishedDate()
            issueDto.newBlogs.first().author == blog.getAuthor()
            issueDto.posts.first().title == post.getTitle()
    }

    def "Should convert blog post to its DTO representation"() {
        given:
            Blog blog = new Blog(1, 2, "some author", "some rss", "some url",
                    "some twitter", now(), BlogType.PERSONAL, true)
            BlogPost post = new BlogPost(1, "some title", "some description", "some url", now(), true, blog)
        when:
            BlogPostDto blogPostJson = builder.fromBlogPost(post)
        then:
            blogPostJson.url == SHORT_URL
            blogPostJson.authorName == blog.getAuthor()
            blogPostJson.authorTwitterHandle == blog.getTwitter()
            blogPostJson.blogType == BlogTypeDto.fromBlogType(blog.getBlogType())
            blogPostJson.title == post.getTitle()
    }

}
