package com.jvm_bloggers.entities.blog_post

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.utils.NowProvider
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.domain.PageRequest
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalDateTime

import static com.jvm_bloggers.core.rss.AggregatedRssFeedProducer.INCLUDE_ALL_AUTHORS_SET
import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

class BlogPostRepositorySpec extends SpringContextAwareSpecification {

    static NOT_MODERATED = null
    static APPROVED = Boolean.TRUE
    static REJECTED = Boolean.FALSE
    static PAGEABLE = new PageRequest(0, Integer.MAX_VALUE)
    static EXCLUDED_AUTHOR = "Excluded Author"
    
    @Subject
    @Autowired
    BlogPostRepository blogPostRepository

    @Autowired
    BlogRepository blogRepository

    def "Should order latest posts by moderation and publication date"() {
        given:
            Blog blog = aBlog("Top Blogger", "http://topblogger.pl/")

            List<BlogPost> blogPosts = [
                aBlogPost(1, LocalDateTime.of(2016, 1, 1, 12, 00), REJECTED, blog),
                aBlogPost(2, LocalDateTime.of(2016, 1, 4, 12, 00), REJECTED, blog),
                aBlogPost(3, LocalDateTime.of(2016, 1, 2, 12, 00), APPROVED, blog),
                aBlogPost(4, LocalDateTime.of(2016, 1, 5, 12, 00), APPROVED, blog),
                aBlogPost(5, LocalDateTime.of(2016, 1, 3, 12, 00), NOT_MODERATED, blog),
                aBlogPost(6, LocalDateTime.of(2016, 1, 6, 12, 00), NOT_MODERATED, blog)
            ]

            blogPostRepository.save(blogPosts)
        when:
            List<BlogPost> latestPosts = blogPostRepository.findLatestPosts(PAGEABLE)
        then:
            // not moderated posts first ...
            !latestPosts[0].isModerated()
            !latestPosts[1].isModerated()
            latestPosts[0].getPublishedDate().isAfter(latestPosts[1].getPublishedDate())

            // ... then moderated ones ordered by published date regardless of an approval
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
            Blog excludedBlog = aBlog(EXCLUDED_AUTHOR, "http://excluded.pl/")
            LocalDateTime publishedDate = new NowProvider().now()
            
            List<BlogPost> excludedblogPosts = [
                aBlogPost(1, publishedDate, REJECTED, excludedBlog),
                aBlogPost(2, publishedDate, APPROVED, excludedBlog),
            ]

            blogPostRepository.save(excludedblogPosts)
            
            Blog includedBlog = aBlog("Included Author", "http://included.pl/")

            List<BlogPost> includedBlogPosts = [
                aBlogPost(3, publishedDate, REJECTED, includedBlog),
                aBlogPost(4, publishedDate, APPROVED, includedBlog),
            ]
            
            blogPostRepository.save(includedBlogPosts)
        when:
            List<BlogPost> filteredPosts = blogPostRepository.findByApprovedTrueAndBlogAuthorNotInOrderByApprovedDateDesc(PAGEABLE, excludedAuthors)
        then:
            filteredPosts.size == expectedPostsCount
        where:
            excludedAuthors                || expectedPostsCount
            [] as Set                      || 0
            [EXCLUDED_AUTHOR] as Set       || 1
            INCLUDE_ALL_AUTHORS_SET as Set || 2
    }

    private Blog aBlog(String author, String rssUrl) {
        return blogRepository.save(
                Blog.builder()
                        .jsonId(1L)
                        .author(author)
                        .rss(rssUrl)
                        .url("url")
                        .dateAdded(LocalDateTime.now())
                        .blogType(PERSONAL)
                        .build());
    }

    private BlogPost aBlogPost(final int index, final LocalDateTime publishedDate,
                               final Boolean approved, final Blog blog) {
        return BlogPost.builder()
                .publishedDate(publishedDate)
                .approved(approved)
                .blog(blog)
                .title("title" + index)
                .url("url" + index)
                .build();
    }
}
