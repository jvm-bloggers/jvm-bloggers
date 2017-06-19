package com.jvm_bloggers.domain.query.blog_statistics_for_listing

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import javaslang.collection.List
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.domain.PageRequest
import spock.lang.Subject

import java.time.LocalDateTime

import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

class BlogStatisticsForListingQueryCacheSpec extends SpringContextAwareSpecification {

    @Autowired
    BlogRepository blogRepository

    @Autowired
    BlogPostRepository blogPostRepository

    @Subject
    @Autowired
    BlogStatisticsForListingQuery blogStatisticsForListingQuery

    def "Should use cached query results"() {
        when:
        List<BlogStatisticsForListing> firstResult = blogStatisticsForListingQuery
                .findBlogPostStatistics(PERSONAL, new PageRequest(0, 1))
        List<BlogStatisticsForListing> secondResult = blogStatisticsForListingQuery
                .findBlogPostStatistics(PERSONAL, new PageRequest(0, 1))

        then:
        firstResult == secondResult
    }

    def "Should invalidate query result cache"() {
        when:
        List<BlogStatisticsForListing> firstResult = blogStatisticsForListingQuery
                .findBlogPostStatistics(PERSONAL, new PageRequest(0, 1))
        Blog blog = blogRepository.save(Blog.builder()
                .jsonId(1L)
                .author("author")
                .rss("rss")
                .url("url")
                .dateAdded(LocalDateTime.now())
                .blogType(PERSONAL)
                .build())
        blogPostRepository.saveAndFlush(BlogPost.builder()
                .publishedDate(LocalDateTime.now())
                .blog(blog)
                .title("title")
                .url("url")
                .build())
        List<BlogStatisticsForListing> secondResult = blogStatisticsForListingQuery
                .findBlogPostStatistics(PERSONAL, new PageRequest(0, 1))

        then:
        firstResult != secondResult
    }
}
