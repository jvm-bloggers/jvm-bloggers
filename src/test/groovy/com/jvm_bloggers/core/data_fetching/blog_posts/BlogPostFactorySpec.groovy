package com.jvm_bloggers.core.data_fetching.blog_posts

import com.jvm_bloggers.TestTimeProvider
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.utils.NowProvider
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime
import java.time.Month

@Subject(BlogPostFactory)
class BlogPostFactorySpec extends Specification {
    private final static int MAX_NEW_POST_AGE_DAYS = 30;
    private final static LocalDateTime NOW = LocalDateTime.of(2016, Month.MARCH, 11, 12, 0, 0)

    NowProvider nowProvider = new TestTimeProvider(NOW)
    Blog blog = Stub(Blog)

    BlogPostFactory blogPostFactory = new BlogPostFactory(MAX_NEW_POST_AGE_DAYS, nowProvider);

    def "Should create blog post"() {
        given:
        blog.getInitialApprovedValue() >> true
        String title = 'title'
        String url = 'url'
        LocalDateTime publishedDate = LocalDateTime.now()

        when:
        BlogPost blogPost = blogPostFactory.create(title, url, publishedDate, blog)

        then:
        blogPost.title == title
        blogPost.url == url
        blogPost.blog == blog
        blogPost.publishedDate == publishedDate
        blogPost.approved
    }

    def "Should set approved according to default approved blog value"() {
        given:
        blog.getInitialApprovedValue() >> defaultApprovedValue

        when:
        BlogPost blogPost = blogPostFactory.create('any title', 'any url', LocalDateTime.now(), blog)

        then:
        blogPost.approved == defaultApprovedValue

        where:
        defaultApprovedValue << [Boolean.TRUE, Boolean.FALSE]
    }

    def "Should set now as approved date for new posts or publishedDate for old posts"() {
        given:
        blog.getInitialApprovedValue() >> true

        when:
        BlogPost blogPost = blogPostFactory.create('any title', 'any url', publishedDate, blog)

        then:
        blogPost.approvedDate == expectedApprovedDate

        where:
        publishedDate     || expectedApprovedDate
        NOW.minusDays(10) || NOW
        NOW.minusDays(50) || NOW.minusDays(50)
    }
}
