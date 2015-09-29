package pl.tomaszdziurko.jvm_bloggers.mailing

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.SpringApplicationContextLoader
import org.springframework.test.context.ContextConfiguration
import pl.tomaszdziurko.jvm_bloggers.JvmBloggersApplication
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.people.domain.Person
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
            List<BlogPost> newPosts = [stubBlogPost("New blog post", "http://example.com/particularPost", "Piotr Nowak")]
        expect:
            blogSummaryMailGenerator.generateSummaryMail(newPosts,[],  7) != null
    }

    def "Should populate template with posts/blog data when new blogs found"() {
        given:
            List<BlogPost> newPosts = [stubBlogPost("New blog post", "http://example.com", "John Travoltowski")]
            List<Person> newBlogs = [stubPerson("John Travoltowski", "http://example.com/rss", "http://example.com/")]
            String mail =""

        when:
            mail = blogSummaryMailGenerator.generateSummaryMail(newPosts,newBlogs,  7)

        then:
            mail != null
            mail.contains("<a href=\"http://example.com/\">John Travoltowski</a><br/>")
    }

    private stubBlogPost(String title, String postUrl, String authorName) {
        return Stub(BlogPost) {
            getTitle() >> title
            getUrl() >> postUrl
            getAuthor() >> Stub(Person) {
                getName() >> authorName
                getTwitter() >> null
            }
        }
    }
    private stubPerson(String authorName, String rss, String homepage) {
        return Stub(Person) {
            getName() >> authorName
            getRss() >> rss
            getHomepage() >> homepage
        }
    }
}