package com.jvm_bloggers.core.newsletter_issues.domain

import com.google.common.collect.Lists
import org.springframework.beans.factory.annotation.Autowired
import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPost
import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPostRepository
import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogRepository
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogType
import com.jvm_bloggers.utils.NowProvider

import java.time.LocalDateTime

class NewsletterIssueRepositorySpec extends SpringContextAwareSpecification {

    @Autowired
    NewsletterIssueRepository newsletterIssueRepository

    @Autowired
    BlogRepository blogRepository

    @Autowired
    BlogPostRepository blogPostRepository

    def "Should save new issue"() {
        given:
            String exampleHeading = "Example heading"
            String exampleVaria = "Example varia"
            long issueNumber = 77L
        and:
            List<Blog> blogs = prepareBlogs()
            List<BlogPost> posts = prepareBlogPosts(blogs.get(0), blogs.get(1))
        when:
            NewsletterIssue issue = NewsletterIssue.builder()
                .issueNumber(issueNumber)
                .publishedDate(new NowProvider().today())
                .newBlogs(blogs)
                .blogPosts(posts)
                .heading(exampleHeading)
                .varia(exampleVaria)
                .build();
            newsletterIssueRepository.save(issue)
        then:
            Optional<NewsletterIssue> persistedIssue =
                    newsletterIssueRepository.findByIssueNumber(issue.getIssueNumber())
            persistedIssue.isPresent()
            persistedIssue.get().blogPosts
            persistedIssue.get().issueNumber == issueNumber
            persistedIssue.get().heading == exampleHeading
            persistedIssue.get().varia == exampleVaria
            persistedIssue.get().newBlogs*.id.containsAll(blogs*.id)
            persistedIssue.get().blogPosts*.id.containsAll(posts*.id)
    }

    private List<Blog> prepareBlogs() {
        Blog blog1 = Blog.builder()
                .active(true)
                .author("John Doe")
                .blogType(BlogType.PERSONAL)
                .dateAdded(LocalDateTime.now())
                .jsonId(1L)
                .rss("http://example.com/rss")
                .url("http://example.com")
                .build()
        blogRepository.save(blog1)
        Blog blog2 = Blog.builder()
                .active(true)
                .author("Kate Ryan")
                .blogType(BlogType.COMPANY)
                .dateAdded(LocalDateTime.now())
                .jsonId(2L)
                .rss("http://another-url.com/rss")
                .url("http://another-url.com")
                .build()
        blogRepository.save(blog2)
        return Lists.asList(blog1, blog2)
    }

    private List<BlogPost> prepareBlogPosts(Blog blog1, Blog blog2) {
        BlogPost blogPost1 = createAndSaveBlogPost(blog1, "http://abc.pl/1")
        BlogPost blogPost2 = createAndSaveBlogPost(blog2, "http://abc.pl/2")
        BlogPost blogPost3 = createAndSaveBlogPost(blog2, "http://abc.pl/3")
        BlogPost blogPost4 = createAndSaveBlogPost(blog2, "http://abc.pl/4")
        return Lists.newArrayList(blogPost1, blogPost2, blogPost3, blogPost4)
    }

    private BlogPost createAndSaveBlogPost(Blog blog, String url) {
        BlogPost blogPost = BlogPost
                .builder()
                .approved(true)
                .blog(blog)
                .description("Example description")
                .publishedDate(LocalDateTime.now())
                .title("Example title")
                .url(url)
                .build()
        blogPostRepository.save(blogPost)
        return blogPost
    }
}
