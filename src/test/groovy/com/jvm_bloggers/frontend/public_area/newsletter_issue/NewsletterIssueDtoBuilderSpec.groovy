package com.jvm_bloggers.frontend.public_area.newsletter_issue

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator
import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPost
import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogType
import com.jvm_bloggers.core.newsletter_issues.domain.NewsletterIssue
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDate

import static java.time.LocalDateTime.now

class NewsletterIssueDtoBuilderSpec extends Specification {

    static final String SHORT_URL = "http://shortlink.com"

    LinkGenerator generator = Stub(LinkGenerator) {
        generateRedirectLinkFor(_ as String ) >> SHORT_URL
    }

    @Subject
    NewsletterIssueDtoBuilder builder = new NewsletterIssueDtoBuilder(generator)

    def "Should convert newsletter issue to its DTO representation"() {
        given:
            Blog blog = sampleBlog()
            BlogPost post = sampleBlogPost(blog)
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
            Blog blog = sampleBlog()
            BlogPost post = sampleBlogPost(blog)
        when:
            BlogPostDto blogPostJson = builder.fromBlogPost(post)
        then:
            blogPostJson.url == SHORT_URL
            blogPostJson.authorName == blog.getAuthor()
            blogPostJson.authorTwitterHandle == blog.getTwitter()
            blogPostJson.blogType == BlogTypeDto.fromBlogType(blog.getBlogType())
            blogPostJson.title == post.getTitle()
    }

    private BlogPost sampleBlogPost(Blog blog) {
        BlogPost.builder()
                .id(1)
                .title("some title")
                .description("some description")
                .url("some url")
                .publishedDate(now())
                .approvedDate(now())
                .approved(true)
                .blog(blog)
                .build()
    }

    private Blog sampleBlog(){
        Blog.builder()
                .id(1)
                .jsonId(2)
                .author("some author")
                .rss("some rss")
                .url("some url")
                .twitter("some twitter")
                .dateAdded(now())
                .blogType(BlogType.PERSONAL)
                .active(true)
                .build()
    }
}
