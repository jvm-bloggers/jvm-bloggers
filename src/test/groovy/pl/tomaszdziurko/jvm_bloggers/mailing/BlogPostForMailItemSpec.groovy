package pl.tomaszdziurko.jvm_bloggers.mailing

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.people.domain.Person
import spock.lang.Specification

class BlogPostForMailItemSpec extends Specification {

    def "Should store author name as a label if twitter account is missing"() {
        given:
            String name = "Jan Kowalski"
            Person author = stubAuthorWith(name, null)
            BlogPost post = Stub(BlogPost) {
                getAuthor() >> author
            }
        when:
            BlogPostForMailItem blogPostForMailItem = new BlogPostForMailItem(post)
        then:
            blogPostForMailItem.authorLabel == name
    }

    def "Should store link to author's twitter account as author label"() {
        given:
            String name = "Jan Kowalski"
            String twitter = "@JanKowalski"
            Person author = stubAuthorWith(name, twitter)
            BlogPost post = Stub(BlogPost) {
                getAuthor() >> author
            }
        when:
            BlogPostForMailItem blogPostForMailItem = new BlogPostForMailItem(post)
        then:
            blogPostForMailItem.authorLabel == "<a href=\"https://twitter.com/" + twitter.substring(1) + "\">" + name + "</a>"
    }

    private stubAuthorWith(String name, String twitterHandler) {
        return Stub(Person) {
            getName() >> name
            getTwitter() >> twitterHandler
        }
    }
}
