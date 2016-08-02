package pl.tomaszdziurko.jvm_bloggers.rest.newsletter_issue

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType
import spock.lang.Specification

import static java.time.LocalDateTime.now

class BlogPostDtoSpec extends Specification {

    def "Should convert blog post to its DTO representation"() {
        given:
            Blog blog = new Blog(1, 2, "some author", "some rss", "some url",
                    "some twitter", now(), BlogType.PERSONAL, true)
            BlogPost post = new BlogPost(1, "some title", "some description", "some url", now(), true, blog)
        when:
            BlogPostDto blogPostJson = BlogPostDto.fromBlogPost(post)
        then:
            blogPostJson.url == post.getUrl()
            blogPostJson.authorName == blog.getAuthor()
            blogPostJson.authorTwitterHandle == blog.getTwitter()
            blogPostJson.blogType == BlogTypeDto.fromBlogType(blog.getBlogType())
            blogPostJson.title == post.getTitle()
    }
}
