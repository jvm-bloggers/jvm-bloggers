package pl.tomaszdziurko.jvm_bloggers.blog_posts.domain

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.domain.PageRequest
import pl.tomaszdziurko.jvm_bloggers.SpringContextAwareSpecification
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalDateTime

import static pl.tomaszdziurko.jvm_bloggers.blog_posts.AggregatedRssFeedProducer.INCLUDE_ALL_AUTHORS_SET
import static pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType.PERSONAL

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
            List<BlogPost> filteredPosts = blogPostRepository.findByApprovedTrueAndBlogAuthorNotInOrderByPublishedDateDesc(PAGEABLE, excludedAuthors)
        then:
            filteredPosts.size == expectedPostsCount
        where:
            excludedAuthors                || expectedPostsCount
            [] as Set                      || 0
            [EXCLUDED_AUTHOR] as Set       || 1
            INCLUDE_ALL_AUTHORS_SET as Set || 2
    }

    @Unroll
    def "Should find #expectedCount blogPosts with url longer than #maxLength and limit #limit"() {
        given:
            Blog blog = aBlog("Any Author", "http://any.com")
            blogPostRepository.save(aBlogPost(1, LocalDateTime.now(), true, blog))
            blogPostRepository.save(aBlogPost(2, LocalDateTime.now(), true, blog))
            blogPostRepository.save(aBlogPost(3, LocalDateTime.now(), true, blog))
            blogPostRepository.save(aBlogPost(4, LocalDateTime.now(), true, blog))
        when:
            List<BlogPost> postsToShortenUid = blogPostRepository.findPostsWithUidLongerThan(maxLength, new PageRequest(0, limit))
        then:
            postsToShortenUid.size() == expectedCount
        where:
            maxLength               | limit || expectedCount
            BlogPost.UID_LENGTH     | 10    || 0
            BlogPost.UID_LENGTH - 1 | 10    || 4
            BlogPost.UID_LENGTH - 1 | 2     || 2
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
