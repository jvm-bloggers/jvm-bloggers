package com.jvm_bloggers.core.newsletter_issues

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.BlogRepositorySpec
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository
import com.jvm_bloggers.utils.NowProvider
import io.vavr.collection.List
import org.springframework.beans.factory.annotation.Autowired
import spock.lang.Subject

import java.time.LocalDateTime

@Subject(InitialDevelopmentIssuePublisher)
class InitialDevelopmentIssuePublisherSpec extends SpringContextAwareSpecification {

    @Autowired
    BlogRepository blogRepository

    NewNewsletterIssuePublisher newNewsletterIssuePublisher = Mock()
    NewsletterIssueRepository newsletterIssueRepository = Mock()
    BlogPostRepository blogPostRepository = Mock()

    InitialDevelopmentIssuePublisher initialDevelopmentIssuePublisher =
            new InitialDevelopmentIssuePublisher(newNewsletterIssuePublisher, newsletterIssueRepository, blogPostRepository)

    def "Shouldn't post a dev issue when there aren't enough blog posts"() {
        given:
        newsletterIssueRepository.count() >> 0
        blogPostRepository.findBlogPostsOfType(BlogType.COMPANY) >> List.empty()
        blogPostRepository.findBlogPostsOfType(BlogType.VIDEOS) >> List.empty()
        blogPostRepository.findBlogPostsOfType(BlogType.PERSONAL) >> List.empty()

        when:
        initialDevelopmentIssuePublisher.publishTestDevelopmentIssue()

        then:
        0 * newNewsletterIssuePublisher.publishNewIssue(_)
    }

    def "Shouldn't post a dev issue when there is an issue published"() {
        given:
        newsletterIssueRepository.count() >> 1

        when:
        initialDevelopmentIssuePublisher.publishTestDevelopmentIssue()

        then:
        0 * newNewsletterIssuePublisher.publishNewIssue(_)
    }

    //TODO
    def "Should post an issue when enough posts appear"() {
        java.util.List<BlogPost> companyBlogPosts = new ArrayList<>()
        java.util.List<BlogPost> personalBlogPosts = new ArrayList<>()
        java.util.List<BlogPost> videoBlogPosts = new ArrayList<>()
        for (int i = 0; i < 15; i++) {
            companyBlogPosts.add(aBlogPost(i,BlogType.COMPANY))
            personalBlogPosts.add(aBlogPost(i,BlogType.PERSONAL))
            videoBlogPosts.add(aBlogPost(i,BlogType.VIDEOS))
        }

        given:
        newsletterIssueRepository.count() >> 0
        2 * blogPostRepository.findBlogPostsOfType(BlogType.COMPANY) >> List.empty()
        2 * blogPostRepository.findBlogPostsOfType(BlogType.VIDEOS) >> List.empty()
        2 * blogPostRepository.findBlogPostsOfType(BlogType.PERSONAL) >> List.empty()
        1 * blogPostRepository.findBlogPostsOfType(BlogType.COMPANY) >> List.ofAll(companyBlogPosts)
        1 * blogPostRepository.findBlogPostsOfType(BlogType.VIDEOS) >> List.ofAll(personalBlogPosts)
        1 * blogPostRepository.findBlogPostsOfType(BlogType.PERSONAL) >> List.ofAll(videoBlogPosts)

        when:
        initialDevelopmentIssuePublisher.publishTestDevelopmentIssue()

        then:
        1 * newNewsletterIssuePublisher.publishNewIssue(_)
    }

    private Blog aBlog(String bookmarkableId, BlogType blogType) {
                return Blog.builder()
                        .bookmarkableId(bookmarkableId)
                        .author("testAuthor")
                        .rss("testRssUrl")
                        .url("url")
                        .dateAdded(LocalDateTime.now())
                        .blogType(blogType)
                        .moderationRequired(false)
                        .build()
    }

    private BlogPost aBlogPost(final int index, final BlogType blogType) {
        Blog blog =  Blog.builder()
                .bookmarkableId(String.valueOf(index))
                .author("testAuthor")
                .rss("testRssUrl")
                .url("url")
                .dateAdded(LocalDateTime.now())
                .blogType(blogType)
                .moderationRequired(false)
                .build();
        return BlogPost.builder()
                .publishedDate(LocalDateTime.now())
                .approved(true)
                .blog(blog)
                .title("title" + index)
                .url("url" + index)
                .build()
    }

}
