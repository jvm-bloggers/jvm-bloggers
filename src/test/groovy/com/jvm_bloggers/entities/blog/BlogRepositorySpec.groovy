package com.jvm_bloggers.entities.blog

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.projections.BlogStatisticsProjection
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import io.vavr.collection.List
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.domain.PageRequest
import spock.lang.Subject

import java.time.LocalDate
import java.time.LocalDateTime

import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

class BlogRepositorySpec extends SpringContextAwareSpecification {

    @Subject
    @Autowired
    BlogRepository blogRepository

    @Autowired
    BlogPostRepository blogPostRepository

    def "Should return blog statistics"() {
        given:
        Blog pBlog1 = aBlog(1, PERSONAL)
        Blog pBlog2 = aBlog(2, PERSONAL)
        Blog pBlog3 = aBlog(3, PERSONAL, false)

        aBlogPost(1, LocalDateTime.now(), pBlog1)
        aBlogPost(2, LocalDateTime.now().minusMonths(1), pBlog1)
        aBlogPost(3, LocalDateTime.now().minusMonths(2), pBlog1)
        aBlogPost(4, LocalDateTime.now().minusMonths(3).minusDays(1), pBlog1)
        aBlogPost(5, LocalDateTime.now().minusMonths(3).plusDays(1), pBlog1)
        aBlogPost(6, LocalDateTime.now().minusMonths(5), pBlog1)
        aBlogPost(7, LocalDateTime.now().minusMonths(6).minusDays(1), pBlog1)
        aBlogPost(8, LocalDateTime.now().minusMonths(6).plusDays(2), pBlog1)
        aBlogPost(9, LocalDateTime.now().minusMonths(6), pBlog3)
        aBlogPost(10, LocalDateTime.now().minusMonths(6).plusDays(1), pBlog2)

        when:
        List<BlogStatisticsProjection> result = blogRepository.findBlogStatistics(
                LocalDate.now().minusMonths(3).atStartOfDay(),
                LocalDate.now().minusMonths(6).atStartOfDay(),
                PERSONAL,
                PageRequest.of(0, 10))

        then:
        result.length() == 2
        result.get(0).firstCount == 4
        result.get(0).secondCount == 7
        result.get(1).firstCount == 0
        result.get(1).secondCount == 1
    }

    private Blog aBlog(int index, BlogType blogType, boolean active = true) {
        return blogRepository.save(
                Blog.builder()
                        .bookmarkableId("bookmarkableId $index")
                        .author("author")
                        .rss("rss $index")
                        .url("url $index")
                        .dateAdded(LocalDateTime.now())
                        .blogType(blogType)
                        .active(active)
                        .moderationRequired(false)
                        .build())
    }

    private BlogPost aBlogPost(int index, LocalDateTime publishedDate, Blog blog) {
        return blogPostRepository.save(BlogPost.builder()
                .publishedDate(publishedDate)
                .blog(blog)
                .title("title $index")
                .url("url $index")
                .approved(true)
                .build())
    }
}
