package com.jvm_bloggers.entities.click

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import io.vavr.collection.List
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.domain.PageRequest
import spock.lang.Subject

import java.time.LocalDateTime

import static com.jvm_bloggers.entities.blog.BlogType.COMPANY
import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

class ClickRepositorySpec extends SpringContextAwareSpecification {

    @Subject
    @Autowired
    ClickRepository clickRepository

    @Autowired
    BlogRepository blogRepository

    @Autowired
    BlogPostRepository blogPostRepository

    def "Should return top clicks for post aggregated by ip address"() {
        given:
        Blog personalBlog = aBlog(1, PERSONAL)
        Blog companyBlog = aBlog(2, COMPANY)

        BlogPost blogPostForPersonalBlog = aBlogPost(1, LocalDateTime.now().minusMonths(1), personalBlog)
        BlogPost blogPostForPersonalBlog_2 = aBlogPost(2, LocalDateTime.now().minusMonths(1), personalBlog)
        BlogPost blogPostForCompanyBlog = aBlogPost(3, LocalDateTime.now().minusMonths(1), companyBlog)

        aClick(blogPostForPersonalBlog, LocalDateTime.now(), '127.0.0.1', 'userAgent')
        aClick(blogPostForPersonalBlog, LocalDateTime.now(), '127.0.0.1', 'userAgent')
        aClick(blogPostForPersonalBlog, LocalDateTime.now(), '127.0.0.2', 'userAgent')
        aClick(blogPostForPersonalBlog, LocalDateTime.now().minusMonths(1), '127.0.0.1', 'userAgent')
        aClick(blogPostForPersonalBlog_2, LocalDateTime.now().minusMonths(1), '127.0.0.1', 'userAgent')
        aClick(blogPostForPersonalBlog_2, LocalDateTime.now().minusMonths(1), '127.0.0.2', 'userAgent')
        aClick(blogPostForPersonalBlog_2, LocalDateTime.now(), '127.0.0.2', 'userAgent')
        aClick(blogPostForCompanyBlog, LocalDateTime.now(), '127.0.0.1', 'userAgent')
        aClick(blogPostForCompanyBlog, LocalDateTime.now(), '127.0.0.2', 'userAgent')

        when:
        List<PostClicksCountByIpAddress> clicksByIpAddress = clickRepository.topPostsClicksCountByIpAddress(
                LocalDateTime.now().minusDays(1),
                LocalDateTime.now().plusDays(1),
                new PageRequest(0, 10)
        )

        then:
        clicksByIpAddress.size() == 5
        clicksByIpAddress.get(0).counter == 2
        clicksByIpAddress.get(1).counter == 1
        clicksByIpAddress.get(2).counter == 1
        clicksByIpAddress.get(3).counter == 1
        clicksByIpAddress.get(4).counter == 1
        clicksByIpAddress.get(4).title == 'title 3'
    }

    def "Should return top clicks for post aggregated by user agent"() {
        given:
        Blog personalBlog = aBlog(1, PERSONAL)
        Blog companyBlog = aBlog(2, COMPANY)

        BlogPost blogPostForPersonalBlog = aBlogPost(1, LocalDateTime.now().minusMonths(1), personalBlog)
        BlogPost blogPostForPersonalBlog_2 = aBlogPost(2, LocalDateTime.now().minusMonths(1), personalBlog)
        BlogPost blogPostForCompanyBlog = aBlogPost(3, LocalDateTime.now().minusMonths(1), companyBlog)

        aClick(blogPostForPersonalBlog, LocalDateTime.now(), '127.0.0.1', '')
        aClick(blogPostForPersonalBlog, LocalDateTime.now(), '127.0.0.2', '')
        aClick(blogPostForPersonalBlog, LocalDateTime.now(), '127.0.0.3', 'userAgent')
        aClick(blogPostForPersonalBlog, LocalDateTime.now().minusMonths(1), '127.0.0.4', 'userAgent')
        aClick(blogPostForPersonalBlog_2, LocalDateTime.now().minusMonths(1), '127.0.0.5', 'userAgent')
        aClick(blogPostForPersonalBlog_2, LocalDateTime.now().minusMonths(1), '127.0.0.6', 'userAgent')
        aClick(blogPostForPersonalBlog_2, LocalDateTime.now(), '127.0.0.7', 'userAgent')
        aClick(blogPostForCompanyBlog, LocalDateTime.now(), '127.0.0.8', 'userAgent')
        aClick(blogPostForCompanyBlog, LocalDateTime.now(), '127.0.0.9', 'userAgent')

        when:
        List<PostClicksCountByUserAgent> clicksByIpAddress = clickRepository.topPostsClicksCountByUserAgent(
                LocalDateTime.now().minusDays(1),
                LocalDateTime.now().plusDays(1),
                new PageRequest(0, 10)
        )

        then:
        clicksByIpAddress.size() == 4
        clicksByIpAddress.get(0).counter == 2
        clicksByIpAddress.get(0).userAgent == ''
        clicksByIpAddress.get(1).counter == 2
        clicksByIpAddress.get(1).userAgent == 'userAgent'
        clicksByIpAddress.get(2).counter == 1
        clicksByIpAddress.get(3).counter == 1
        clicksByIpAddress.get(3).title == 'title 2'
    }

    private Click aClick(BlogPost blogPost, LocalDateTime clickDate, String ipAddress, String userAgent) {
        return clickRepository.save(new Click(blogPost, clickDate, ipAddress, 'referer', userAgent))
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