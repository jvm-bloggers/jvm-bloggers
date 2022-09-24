package com.jvm_bloggers.core.data_fetching.blog_posts

import com.jvm_bloggers.core.utils.EmojiRemover
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog_post.BlogPost
import spock.lang.Ignore
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime
import java.time.Month

@Subject(BlogPostFactory)
class BlogPostFactorySpec extends Specification {
    private final static LocalDateTime NOW = LocalDateTime.of(2016, Month.MARCH, 11, 12, 0, 0)

    private final Blog blog = Stub(Blog)
    private final EmojiRemover remover = Mock(EmojiRemover)

    private final BlogPostFactory blogPostFactory = new BlogPostFactory(remover)

    def "Should create blog post"() {
        given:
        blog.getInitialApprovedValue() >> true
        String title = 'title'
        String url = 'url'
        LocalDateTime publishedDate = LocalDateTime.now()

        1 * remover.remove(title) >> 'cleaned_title'

        when:
        BlogPost blogPost = blogPostFactory.create(title, url, publishedDate, blog)

        then:
        blogPost.title == 'cleaned_title'
        blogPost.url == url
        blogPost.blog == blog
        blogPost.publishedDate == publishedDate
        blogPost.approved
    }

    def "Should set approved according to default approved blog value"() {
        given:
        blog.getInitialApprovedValue() >> defaultApprovedValue
        1 * remover.remove(_) >> 'any title'

        when:
        BlogPost blogPost = blogPostFactory.create('any title', 'any url', LocalDateTime.now(), blog)

        then:
        blogPost.approved == defaultApprovedValue

        where:
        defaultApprovedValue << [Boolean.TRUE, Boolean.FALSE]
    }

    @Ignore("Temporarily disable this test to fetch as many old posts without affecting current newsletter issues")
    def "Should set now as approved date for new posts or publishedDate for old posts"() {
        given:
        blog.getInitialApprovedValue() >> true
        1 * remover.remove(_) >> 'any title'

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
