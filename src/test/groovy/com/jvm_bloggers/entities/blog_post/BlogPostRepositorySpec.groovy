package com.jvm_bloggers.entities.blog_post

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.utils.NowProvider
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.domain.PageRequest
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalDateTime

import static com.jvm_bloggers.ObjectMother.aBlog
import static com.jvm_bloggers.ObjectMother.aBlogPost
import static com.jvm_bloggers.core.rss.AggregatedRssFeedProducer.INCLUDE_ALL_AUTHORS_SET
import static java.lang.Boolean.FALSE
import static java.lang.Boolean.TRUE
import static java.lang.Integer.MAX_VALUE

@Subject(BlogPostRepository)
class BlogPostRepositorySpec extends SpringContextAwareSpecification {

    static NOT_MODERATED = null
    static APPROVED = TRUE
    static REJECTED = FALSE
    static PAGEABLE = PageRequest.of(0, MAX_VALUE)

    static EXCLUDED_AUTHOR = "Excluded Author"

    @Autowired
    BlogPostRepository blogPostRepository

    @Autowired
    BlogRepository blogRepository

    def "Should order latest posts by moderation and publication date"() {
        given:
        Blog blog = blogRepository.save(aBlog())
        LocalDateTime baseTime = LocalDateTime.of(2016, 1, 1, 12, 00)

        List<BlogPost> blogPosts = [
                aBlogPost(publishedDate: baseTime.plusDays(0), approved: REJECTED, blog: blog),
                aBlogPost(publishedDate: baseTime.plusDays(1), approved: REJECTED, blog: blog),
                aBlogPost(publishedDate: baseTime.plusDays(2), approved: APPROVED, blog: blog),
                aBlogPost(publishedDate: baseTime.plusDays(3), approved: APPROVED, blog: blog),
                aBlogPost(publishedDate: baseTime.plusDays(4), approved: NOT_MODERATED, blog: blog),
                aBlogPost(publishedDate: baseTime.plusDays(5), approved: NOT_MODERATED, blog: blog)
        ]

        blogPostRepository.saveAll(blogPosts)

        when:
        List<BlogPost> latestPosts = blogPostRepository.findLatestPosts(PAGEABLE)

        then: 'not moderated posts first'
        !latestPosts[0].isModerated()
        !latestPosts[1].isModerated()
        latestPosts[0].getPublishedDate().isAfter(latestPosts[1].getPublishedDate())

        and: 'then moderated ones ordered by published date regardless of an approval'
        latestPosts[2].isModerated()
        latestPosts[3].isModerated()
        latestPosts[4].isModerated()
        latestPosts[5].isModerated()

        latestPosts[2].getPublishedDate().isAfter(latestPosts[3].getPublishedDate())
        latestPosts[3].getPublishedDate().isAfter(latestPosts[4].getPublishedDate())
        latestPosts[4].getPublishedDate().isAfter(latestPosts[5].getPublishedDate())
    }

    @Unroll
    def "Should filter out posts by authors = #excludedAuthors"() {
        given:
        Blog excludedBlog = blogRepository.save(aBlog(author: EXCLUDED_AUTHOR))
        Blog includedBlog = blogRepository.save(aBlog(author: 'Included Author'))

        List<BlogPost> excludedblogPosts = [
                aBlogPost(approved: REJECTED, blog: excludedBlog),
                aBlogPost(approved: APPROVED, blog: excludedBlog)
        ]

        List<BlogPost> includedBlogPosts = [
                aBlogPost(approved: REJECTED, blog: includedBlog),
                aBlogPost(approved: APPROVED, blog: includedBlog),
        ]

        blogPostRepository.saveAll(includedBlogPosts + excludedblogPosts)

        when:
        List<BlogPost> filteredPosts = blogPostRepository.
                findByApprovedTrueAndBlogAuthorNotInOrderByApprovedDateDesc(PAGEABLE, excludedAuthors)

        then:
        filteredPosts.size == expectedPostsCount

        where:
        excludedAuthors                || expectedPostsCount
        [] as Set                      || 0
        [EXCLUDED_AUTHOR] as Set       || 1
        INCLUDE_ALL_AUTHORS_SET as Set || 2
    }

    def "Should count blog posts with BlogType = #blogType"() {
        given:
        Blog companyBlog = aBlog(blogType: BlogType.COMPANY)
        Blog videoBlog = aBlog(blogType: BlogType.VIDEOS)
        Blog personalBlog = aBlog(blogType: BlogType.PERSONAL)

        List<BlogPost> blogPosts = [
                aBlogPost(blog:  personalBlog),
                aBlogPost(blog: videoBlog),
                aBlogPost(blog: personalBlog),
                aBlogPost(blog: companyBlog),
                aBlogPost(blog: personalBlog),
                aBlogPost(blog: companyBlog)
        ]

        blogPostRepository.saveAll(blogPosts);

        when:
        List<BlogPost> blogPostsByBlogType = blogPostRepository.selectBlogPostsOfType(blogType)

        then:
        blogPostsByBlogType.size() == expectedBlogPostCount

        where:
        blogType          || expectedBlogPostCount
        BlogType.COMPANY  || 2
        BlogType.VIDEOS   || 1
        BlogType.PERSONAL || 3
    }

}
