package pl.tomaszdziurko.jvm_bloggers.blog_posts.domain

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.domain.PageRequest
import org.springframework.test.context.ActiveProfiles
import pl.tomaszdziurko.jvm_bloggers.SpringContextAwareSpecification
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository

import java.time.LocalDateTime

import static pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType.PERSONAL

class BlogPostRepositorySpec extends SpringContextAwareSpecification {

    static NOT_MODERATED = null
    static APPROVED = Boolean.TRUE
    static REJECTED = Boolean.FALSE

    @Autowired
    BlogPostRepository blogPostRepository

    @Autowired
    BlogRepository blogRepository

    def "Should order latest posts by moderation and publication date"() {
        given:
            Blog blog = aBlog()

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
            List<BlogPost> latestPosts = blogPostRepository.findLatestPosts(new PageRequest(0, blogPosts.size()))
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

    private Blog aBlog() {
        return blogRepository.save(
                Blog.builder()
                        .jsonId(1L)
                        .author("Top Blogger")
                        .rss("http://topblogger.pl/")
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
