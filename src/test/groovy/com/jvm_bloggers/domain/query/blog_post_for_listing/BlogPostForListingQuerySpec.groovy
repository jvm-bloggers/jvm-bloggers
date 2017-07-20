package com.jvm_bloggers.domain.query.blog_post_for_listing

import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import javaslang.control.Option
import org.springframework.data.domain.PageRequest
import spock.lang.Specification
import spock.lang.Subject

import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL
import static java.time.LocalDateTime.now
import static java.util.UUID.randomUUID

class BlogPostForListingQuerySpec  extends Specification {

    BlogRepository blogRepository = Stub()

    BlogPostRepository blogPostRepository = Stub()

    @Subject
    BlogPostForListingQuery blogPostForListingQuery = new BlogPostForListingQuery(blogRepository, blogPostRepository)

    def "Should query blog by code"() {
        given:
        Long blogId = 1L
        String code = "someCode"
        Blog blog = aBlog()
        blogRepository.findByBookmarkableId(code) >> Option.of(blog)

        when:
        Option<Long> result = blogPostForListingQuery.findBlogIdByCode(code)

        then:
        result.isDefined()
        result.get() == blogId
    }

    def "Should transform Blog to BlogDisplayDetails"() {
        given:
        Long blogId = 1L
        Blog blog = aBlog()
        blogRepository.findOne(blogId) >> blog

        when:
        Option<BlogDisplayDetails> blogDisplayDetailsOption = blogPostForListingQuery.findBlogDisplayDetails(blogId)

        then:
        blogDisplayDetailsOption.isDefined()
        BlogDisplayDetails blogDisplayDetails = blogDisplayDetailsOption.get()
        blogDisplayDetails.author == blog.author
        blogDisplayDetails.type == blog.blogType
        blogDisplayDetails.url == blog.url
    }

    def "Should query posts by blog id"() {
        given:
        Long blogId = 1L
        Blog blog = aBlog()
        List<BlogPost> blogPosts = [
                aBlogPost(blog),
                aBlogPost(blog),
                aBlogPost(blog),
                aBlogPost(blog),
                aBlogPost(blog),
                aBlogPost(blog)
        ]
        blogPostRepository.findByBlogIdOrderByPublishedDateDesc(blogId, new PageRequest(0, 10)) >> blogPosts

        when:
        List<BlogPostForListing> result = blogPostForListingQuery.findBlogPosts(blogId, 0, 10).toJavaList()

        then:
        result.size() == 6
        result[0].uid == blogPosts[0].uid
        result[0].title == blogPosts[0].title
        result[0].publishedDate == blogPosts[0].publishedDate
    }

    private Blog aBlog() {
        new Blog.BlogBuilder()
                .url(randomUUID().toString())
                .id(1)
                .jsonId(1)
                .bookmarkableId(randomUUID().toString())
                .rss(randomUUID().toString())
                .dateAdded(now())
                .author(randomUUID().toString())
                .blogType(PERSONAL)
                .build()
    }

    private BlogPost aBlogPost(final Blog blog) {
        return BlogPost.builder()
                .publishedDate(now())
                .approved(true)
                .blog(blog)
                .title(randomUUID().toString())
                .url(randomUUID().toString())
                .build();
    }
}
