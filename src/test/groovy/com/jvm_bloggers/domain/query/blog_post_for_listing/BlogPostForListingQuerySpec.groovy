package com.jvm_bloggers.domain.query.blog_post_for_listing

import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import io.vavr.control.Option
import org.springframework.data.domain.PageRequest
import spock.lang.Specification
import spock.lang.Subject

import static com.jvm_bloggers.ObjectMother.aBlog
import static com.jvm_bloggers.ObjectMother.aBlogPost

@Subject(BlogPostForListingQuery)
class BlogPostForListingQuerySpec  extends Specification {

    BlogRepository blogRepository = Stub()

    BlogPostRepository blogPostRepository = Stub()

    BlogPostForListingQuery blogPostForListingQuery = new BlogPostForListingQuery(blogRepository, blogPostRepository)

    def "Should query blog by code"() {
        given:
        String code = "someCode"
        Blog blog = aBlog()
        blogRepository.findByBookmarkableId(code) >> Option.of(blog)

        when:
        Option<Long> result = blogPostForListingQuery.findBlogIdByBookmarkableId(code)

        then:
        result.isDefined()
        result.get() == blog.id
    }

    def "Should transform Blog to BlogDisplayDetails"() {
        given:
        Long blogId = 1L
        Blog blog = aBlog()
        blogRepository.findById(blogId) >> Optional.of(blog)

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
        List<BlogPost> blogPosts = [aBlogPost()] * 6

        blogPostRepository.findByBlogIdAndApprovedTrueOrderByPublishedDateDesc(blogId, PageRequest.of(0, 10)) >> blogPosts

        when:
        List<BlogPostForListing> result = blogPostForListingQuery.findBlogPosts(blogId, 0, 10).toJavaList()

        then:
        result.size() == blogPosts.size()
        result[0].uid == blogPosts[0].uid
        result[0].title == blogPosts[0].title
        result[0].publishedDate == blogPosts[0].publishedDate
    }
}
