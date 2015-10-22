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

    def "Should populate template with posts data"() {
        given:
            List<BlogPost> newPosts = [stubBlogPost("New blog post", "http://example.com/postUrl", "Piotr Nowak")]
        expect:
            blogSummaryMailGenerator.generateSummaryMail(newPosts,[],  7) != null
    }

    def "Should populate template with posts/blog data when new blogs found"() {
        given:
            List<BlogPost> newPosts = [stubBlogPost("New blog post", "http://example.com", "Piotr Nowak")]
            List<Blog> newBlogs = [
                    stubPerson("Tomasz Dziurko", "http://tomaszdziurko.pl/feed/"),
                    stubPerson("Tomasz Nurkiewicz", "http://www.nurkiewicz.com/feeds/posts/default?alt=rss"),
            ]
            String mail =""


        when:
            mail = blogSummaryMailGenerator.generateSummaryMail(newPosts,newBlogs,  7)

        then:
            mail != null
            mail.contains("<a href=\"http://tomaszdziurko.pl/\">Tomasz Dziurko</a><br/>")
            mail.contains("<a href=\"http://www.nurkiewicz.com/\">Tomasz Nurkiewicz</a><br/>")
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