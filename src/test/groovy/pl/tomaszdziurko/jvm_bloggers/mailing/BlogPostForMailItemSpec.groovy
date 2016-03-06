package pl.tomaszdziurko.jvm_bloggers.mailing

import java.text.SimpleDateFormat
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import spock.lang.Specification

class BlogPostForMailItemSpec extends Specification {

    def "Should store author name as a label if twitter account is missing"() {
        given:
            String name = "Jan Kowalski"
            Blog author = stubAuthorWith(name, null)
            BlogPost post = Stub(BlogPost) {
                getBlog() >> author
            }
        when:
            BlogPostForMailItem blogPostForMailItem = BlogPostForMailItem.builder().from(post).build()
        then:
            blogPostForMailItem.authorLabel == name
    }

    def "Should store link to author's twitter account as author label"() {
        given:
            String name = "Jan Kowalski"
            String twitter = "@JanKowalski"
            Blog author = stubAuthorWith(name, twitter)
            BlogPost post = Stub(BlogPost) {
                getBlog() >> author
            }
        when:
            BlogPostForMailItem blogPostForMailItem = BlogPostForMailItem.builder().from(post).build()
        then:
            blogPostForMailItem.authorLabel == "<a href=\"https://twitter.com/" + twitter.substring(1) + "\">" + name + "</a>"
    }
    
    def "Should build URL with default UTM parameters"() {
        given:
            Blog author = stubAuthorWith("Jan Kowalski", "@JanKowalski")
            BlogPost post = Stub(BlogPost) {
                getBlog() >> author
                getUrl() >> "http://www.blog.pl"
            }
            long issueNumber = 13;
        when:
            BlogPostForMailItem blogPostForMailItem = BlogPostForMailItem.builder().from(post).withIssueNumber(issueNumber).withDefaultUTMParameters().build()
        then:
            blogPostForMailItem.url == "http://www.blog.pl?utm_source=jvm-bloggers.com&utm_medium=newsletter&utm_campaign=jvm-bloggers#" + issueNumber
    }

    private stubAuthorWith(String name, String twitterHandler) {
        return Stub(Blog) {
            getAuthor() >> name
            getTwitter() >> twitterHandler
        }
    }
}
