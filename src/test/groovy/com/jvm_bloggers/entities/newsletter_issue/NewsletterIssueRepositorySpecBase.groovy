package com.jvm_bloggers.entities.newsletter_issue

import com.google.common.collect.Lists
import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.utils.NowProvider
import org.springframework.beans.factory.annotation.Autowired

import java.time.LocalDateTime

class NewsletterIssueRepositorySpecBase extends SpringContextAwareSpecification {

    @Autowired
    NewsletterIssueRepository newsletterIssueRepository

    @Autowired
    BlogRepository blogRepository

    @Autowired
    BlogPostRepository blogPostRepository

    protected List<Blog> prepareBlogs() {
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

    protected List<BlogPost> prepareBlogPosts(Blog blog1, Blog blog2) {
        BlogPost blogPost1 = createAndSaveBlogPost(blog1, "http://abc.pl/1")
        BlogPost blogPost2 = createAndSaveBlogPost(blog2, "http://abc.pl/2")
        BlogPost blogPost3 = createAndSaveBlogPost(blog2, "http://abc.pl/3")
        BlogPost blogPost4 = createAndSaveBlogPost(blog2, "http://abc.pl/4")
        return Lists.newArrayList(blogPost1, blogPost2, blogPost3, blogPost4)
    }

    protected BlogPost createAndSaveBlogPost(Blog blog, String url) {
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

    protected NewsletterIssue.NewsletterIssueBuilder prepareNewsletterIssue(long issueNumber, List blogs, List posts, String exampleHeading, String exampleVaria) {
        return NewsletterIssue.builder()
                .issueNumber(issueNumber)
                .publishedDate(new NowProvider().today())
                .newBlogs(blogs)
                .blogPosts(posts)
                .heading(exampleHeading)
                .varia(exampleVaria);
    }
    
    protected NewsletterIssue buildNewsletterIssue(long issueNumber, List blogs, List posts, String exampleHeading, String exampleVaria) {
        return prepareNewsletterIssue(issueNumber, blogs, posts, exampleHeading, exampleVaria).build()
    }
}
