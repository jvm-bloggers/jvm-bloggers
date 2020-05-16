package com.jvm_bloggers.entities.newsletter_issue

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.utils.ZoneTimeProvider
import org.springframework.beans.factory.annotation.Autowired

import static com.jvm_bloggers.ObjectMother.aBlog
import static com.jvm_bloggers.ObjectMother.aBlogPost

class NewsletterIssueRepositorySpecBase extends SpringContextAwareSpecification {

    @Autowired
    NewsletterIssueRepository newsletterIssueRepository

    @Autowired
    BlogRepository blogRepository

    @Autowired
    BlogPostRepository blogPostRepository

    protected List<Blog> prepareBlogs() {
        [aBlog(author: 'John Doe'), aBlog(author: 'Kate Ryan')].each {
            blogRepository.save(it)
        }
    }

    protected List<BlogPost> prepareBlogPosts(Blog blog1, Blog blog2) {
        BlogPost blogPost1 = createAndSaveBlogPost(blog1, "http://abc.pl/1")
        BlogPost blogPost2 = createAndSaveBlogPost(blog2, "http://abc.pl/2")
        BlogPost blogPost3 = createAndSaveBlogPost(blog2, "http://abc.pl/3")
        BlogPost blogPost4 = createAndSaveBlogPost(blog2, "http://abc.pl/4")
        return [blogPost1, blogPost2, blogPost3, blogPost4]
    }

    protected BlogPost createAndSaveBlogPost(Blog blog, String url) {
        BlogPost blogPost = aBlogPost(
                blog: blog,
                url: url
        )
        blogPostRepository.save(blogPost)
        return blogPost
    }

    protected NewsletterIssue.NewsletterIssueBuilder prepareNewsletterIssue(long issueNumber, List blogs, List posts, String exampleHeading, String exampleVaria) {
        return NewsletterIssue.builder()
                .issueNumber(issueNumber)
                .publishedDate(new ZoneTimeProvider().today())
                .newBlogs(blogs)
                .blogPosts(posts)
                .heading(exampleHeading)
                .varia(exampleVaria)
    }
    
    protected NewsletterIssue buildNewsletterIssue(long issueNumber, List blogs, List posts, String exampleHeading, String exampleVaria) {
        return prepareNewsletterIssue(issueNumber, blogs, posts, exampleHeading, exampleVaria).build()
    }
}
