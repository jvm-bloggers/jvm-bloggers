package pl.tomaszdziurko.jvm_bloggers.mailing

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.SpringApplicationContextLoader
import org.springframework.test.context.ContextConfiguration
import pl.tomaszdziurko.jvm_bloggers.JvmBloggersApplication
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.people.Person
import spock.lang.Specification

@ContextConfiguration(classes = [JvmBloggersApplication], loader = SpringApplicationContextLoader)
class BlogSummaryMailGeneratorSpec extends Specification {

    @Autowired
    BlogSummaryMailGenerator blogSummaryMailGenerator

    def "Should populate template with posts data"() {
        given:
            List<BlogPost> newPosts = [stubBlogPost("New blog post", "http://example.com", "Piotr Nowak")]
        when:
            String mail = blogSummaryMailGenerator.generateSummaryMail(newPosts, 7)
        then:
            mail != null
            noExceptionThrown()
    }


    private stubBlogPost(String title, String url, String authorName) {
        return Stub(BlogPost) {
            getTitle() >> title
            getUrl() >> url
            getAuthor() >> Stub(Person) {
                getName() >> authorName
                getTwitter() >> null
            }
        }
    }
}
