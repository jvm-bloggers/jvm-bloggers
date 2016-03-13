package pl.tomaszdziurko.jvm_bloggers.blog_posts.domain

import org.springframework.beans.factory.annotation.Autowired
import pl.tomaszdziurko.jvm_bloggers.SpringContextAwareSpecification
import spock.lang.Specification
import java.lang.Void as Should
import java.time.LocalDateTime
import java.time.Month

class BlogPostRepositoryTest extends SpringContextAwareSpecification {

    @Autowired
    private BlogPostRepository blogPostRepository;

    Should "Return not empty collection"() {
        given:
        LocalDateTime publishedDate = LocalDateTime.of(2010, Month.JANUARY, 3, 12, 0)

        when:
        List<BlogPost> blogPosts = blogPostRepository.findByPublishedDateAfterAndApprovedTrueOrderByPublishedDateAsc(publishedDate)

        then:
        !blogPosts.isEmpty()
    }

    Should "Return empty collection"() {
        given:
        LocalDateTime publishedDate = LocalDateTime.of(2017, Month.JANUARY, 3, 12, 0)

        when:
        List<BlogPost> blogPosts = blogPostRepository.findByPublishedDateAfterAndApprovedTrueOrderByPublishedDateAsc(publishedDate)

        then:
        blogPosts.isEmpty()
    }
}