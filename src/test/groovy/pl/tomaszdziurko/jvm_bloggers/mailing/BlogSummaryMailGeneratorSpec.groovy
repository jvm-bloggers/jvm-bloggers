package pl.tomaszdziurko.jvm_bloggers.mailing

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.SpringApplicationContextLoader
import org.springframework.test.context.ContextConfiguration
import pl.tomaszdziurko.jvm_bloggers.JvmBloggersApplication
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import spock.lang.Specification

@ContextConfiguration(classes = [JvmBloggersApplication], loader = SpringApplicationContextLoader)
class BlogSummaryMailGeneratorSpec extends Specification {

    @Autowired
    BlogSummaryMailGenerator blogSummaryMailGenerator

    def setupSpec() {
        System.setProperty("jasypt.encryptor.password", "password")
    }

    def "Should populate template with data"() {
        given:
            List<BlogPost> newPosts = [stubBlogPost("New blog post", "http://example.com/postUrl", "Piotr Nowak")]
            List<BlogPost> companiesPosts = [stubBlogPost("New blog post from company", "http://softwaremill.com/post", "SoftwareMill")]
            List<Blog> newBlogs = [
                    stubPerson("Tomasz Dziurko", "http://tomaszdziurko.pl/feed/"),
                    stubPerson("Tomasz Nurkiewicz", "http://www.nurkiewicz.com/feeds/posts/default?alt=rss"),
            ]
        when:
            String mail = blogSummaryMailGenerator.generateSummaryMail(newPosts, companiesPosts, newBlogs,  7)
        then:
            mail != null
            mail.contains("Wpisy programistów")
            mail.contains(newPosts.get(0).url)
            mail.contains("Wpisy z blogów firmowych")
            mail.contains(companiesPosts.get(0).url)
            mail.contains("Nowo dodane blogi")
            mail.contains("<a href=\"http://tomaszdziurko.pl/\">Tomasz Dziurko</a><br/>")
            mail.contains("<a href=\"http://www.nurkiewicz.com/\">Tomasz Nurkiewicz</a><br/>")
    }

    def "Should hide personal blogs section when there are no new personal posts"() {
        given:
            List<BlogPost> companiesPosts = [stubBlogPost("New blog post from company", "http://softwaremill.com/post", "SoftwareMill")]
        when:
            String mail = blogSummaryMailGenerator.generateSummaryMail([], companiesPosts, [],  7)
        then:
            mail != null
            !mail.contains("Wpisy programistów")
    }

    def "Should hide companies blogs section when there are no new company posts"() {
        given:
            List<BlogPost> newPosts = [stubBlogPost("New blog post", "http://example.com/postUrl", "Piotr Nowak")]
        when:
            String mail = blogSummaryMailGenerator.generateSummaryMail(newPosts, [], [],  7)
        then:
            mail != null
            !mail.contains("Wpisy z blogów firmowych")
    }

    def "Should hide new blogs section when there are no new blogs"() {
        given:
            List<BlogPost> newPosts = [stubBlogPost("New blog post", "http://example.com/postUrl", "Piotr Nowak")]
        when:
            String mail = blogSummaryMailGenerator.generateSummaryMail(newPosts, [], [],  7)
        then:
            mail != null
            !mail.contains("Nowo dodane blogi")
    }

    private stubBlogPost(String title, String url, String authorName) {
        return Stub(BlogPost) {
            getTitle() >> title
            getUrl() >> url
            getBlog() >> Stub(Blog) {
                getAuthor() >> authorName
                getTwitter() >> null
            }
        }
    }
    private stubPerson(String authorName, String rss) {
        return Stub(Blog) {
            getAuthor() >> authorName
            getRss() >> rss
        }
    }
}