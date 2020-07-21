package com.jvm_bloggers.entities.blog_post

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.tag.TagRepository
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.domain.PageRequest
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalDateTime

import static com.jvm_bloggers.ObjectMother.aBlog
import static com.jvm_bloggers.ObjectMother.aBlogPost
import static com.jvm_bloggers.core.rss.AggregatedRssFeedProducer.INCLUDE_ALL_AUTHORS_SET
import static com.jvm_bloggers.entities.blog.BlogType.*
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

    @Autowired
    TagRepository tagRepository

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


    @Unroll
    def "Should return proper amount of unapproved posts with BlogType = #blogType"() {
        given:
        Blog companyBlog = aBlog(blogType: COMPANY)
        Blog podcastBlog = aBlog(blogType: PODCAST)
        Blog presentationBlog = aBlog(blogType: PRESENTATION)

        List<BlogPost> blogPosts = [
                aBlogPost(approved: null, blog: companyBlog),
                aBlogPost(approved: REJECTED, blog: presentationBlog),
                aBlogPost(approved: null, blog: presentationBlog),
                aBlogPost(approved: APPROVED, blog: companyBlog),
                aBlogPost(approved: null, blog: podcastBlog),
                aBlogPost(approved: null, blog: companyBlog),
                aBlogPost(approved: null, blog: companyBlog)
        ]

        blogRepository.save(companyBlog)
        blogRepository.save(podcastBlog)
        blogRepository.save(presentationBlog)
        blogPostRepository.saveAll(blogPosts);

        when:
        List<BlogPost> unapprovedBlogPostsByBlogType = blogPostRepository
                .findUnapprovedPostsByBlogType(blogType, PageRequest.of(0, 3))

        then:
        unapprovedBlogPostsByBlogType.size() == expectedBlogPostCount

        where:
        blogType     || expectedBlogPostCount
        COMPANY      || 3
        PRESENTATION || 1
        PODCAST      || 1
    }

    @Unroll
    def "Should return proper amount of blog posts with BlogType = #blogType"() {
        given:
        Blog companyBlog = aBlog(blogType: COMPANY)
        Blog podcastBlog = aBlog(blogType: PODCAST)
        Blog presentationBlog = aBlog(blogType: PRESENTATION)
        Blog personalBlog = aBlog(blogType: PERSONAL)

        List<BlogPost> blogPosts = [
                aBlogPost(blog: personalBlog),
                aBlogPost(blog: podcastBlog),
                aBlogPost(blog: presentationBlog),
                aBlogPost(blog: personalBlog),
                aBlogPost(blog: companyBlog),
                aBlogPost(blog: personalBlog),
                aBlogPost(blog: companyBlog)
        ]

        blogRepository.save(companyBlog)
        blogRepository.save(presentationBlog)
        blogRepository.save(podcastBlog)
        blogRepository.save(personalBlog)
        blogPostRepository.saveAll(blogPosts);

        when:
        List<BlogPost> blogPostsByBlogType = blogPostRepository
                .findBlogPostsOfType(blogType, PageRequest.of(0, 4))

        then:
        blogPostsByBlogType.size() == expectedBlogPostCount

        where:
        blogType     || expectedBlogPostCount
        COMPANY      || 2
        PRESENTATION || 1
        PRESENTATION || 1
        PERSONAL     || 3
    }

}
