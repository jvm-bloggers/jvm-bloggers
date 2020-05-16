package com.jvm_bloggers.entities.blog

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.projections.BlogStatisticsProjection
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import io.vavr.collection.List
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.domain.PageRequest
import spock.lang.Subject

import static java.time.LocalDate.now as today
import static java.time.LocalDateTime.now
import static com.jvm_bloggers.ObjectMother.aBlog
import static com.jvm_bloggers.ObjectMother.aBlogPost
import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

class BlogRepositorySpec extends SpringContextAwareSpecification {

    @Subject
    @Autowired
    BlogRepository blogRepository

    @Autowired
    BlogPostRepository blogPostRepository

    def "Should return blog statistics"() {
        given:
        Blog pBlog1 = blogRepository.save(aBlog(blogType:  PERSONAL))
        Blog pBlog2 = blogRepository.save(aBlog(blogType:  PERSONAL))
        Blog pBlog3 = blogRepository.save(aBlog(blogType:  PERSONAL, active: false))

        blogPostRepository.with {
            save(aBlogPost(publishedDate: now(), blog: pBlog1))
            save(aBlogPost(publishedDate: now().minusMonths(1), blog: pBlog1))
            save(aBlogPost(publishedDate: now().minusMonths(2), blog: pBlog1))
            save(aBlogPost(publishedDate: now().minusMonths(3).minusDays(1), blog: pBlog1))
            save(aBlogPost(publishedDate: now().minusMonths(3).plusDays(1), blog: pBlog1))
            save(aBlogPost(publishedDate: now().minusMonths(5), blog: pBlog1))
            save(aBlogPost(publishedDate: now().minusMonths(6).minusDays(1), blog: pBlog1))
            save(aBlogPost(publishedDate: now().minusMonths(6).plusDays(2), blog: pBlog1))
            save(aBlogPost(publishedDate: now().minusMonths(6), blog: pBlog3))
            save(aBlogPost(publishedDate: now().minusMonths(6).plusDays(1), blog: pBlog2))
        }


        when:
        List<BlogStatisticsProjection> result = blogRepository.findBlogStatistics(
                today().minusMonths(3).atStartOfDay(),
                today().minusMonths(6).atStartOfDay(),
                PERSONAL,
                PageRequest.of(0, 10))

        then:
        result.length() == 2
        result.get(0).firstCount == 4
        result.get(0).secondCount == 7
        result.get(1).firstCount == 0
        result.get(1).secondCount == 1
    }
}
