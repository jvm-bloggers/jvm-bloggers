package com.jvm_bloggers.core.mailing

import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog_post.BlogPost
import org.apache.commons.lang3.StringUtils
import spock.lang.Specification
import spock.lang.Subject

@Subject(BlogPostForMailItem)
class BlogPostForMailItemSpec extends Specification {

    String url = 'test.pl'
    String title = 'My new article!'

    def "Should store author name as a label if twitter account is missing"() {
        given:
        String name = 'Jan Kowalski'
        Blog author = stubAuthorWith(name, null)
        BlogPost post = Stub(BlogPost) {
            getBlog() >> author
            getUrl() >> url
            getTitle() >> title
        }

        when:
        BlogPostForMailItem blogPostForMailItem = BlogPostForMailItem.builder().from(post).build()

        then:
        blogPostForMailItem.authorLabel == name
    }

    def "Should store link to author's twitter account as author label"() {
        given:
        String name = 'Jan Kowalski'
        String twitter = '@JanKowalski'
        Blog author = stubAuthorWith(name, twitter)
        BlogPost post = Stub(BlogPost) {
            getBlog() >> author
            getUrl() >> url
            getTitle() >> title
        }

        when:
        BlogPostForMailItem blogPostForMailItem = BlogPostForMailItem.builder().from(post).build()

        then:
        blogPostForMailItem.authorLabel == "<a href=\"https://twitter.com/" + twitter.substring(1) + "\">" + name + "</a>"
    }

    def "Should store author name as a label if twitter account is set with empty or null string"() {
        given:
        String name = 'Jan Kowalski'
        String twitter = StringUtils.EMPTY;
        Blog author = stubAuthorWith(name, twitter)
        BlogPost post = Stub(BlogPost) {
            getBlog() >> author
            getUrl() >> url
            getTitle() >> title
        }

        when:
        BlogPostForMailItem blogPostForMailItem = BlogPostForMailItem.builder().from(post).build()

        then:
        blogPostForMailItem.authorLabel == name
    }

    private stubAuthorWith(String name, String twitterHandler) {
        return Stub(Blog) {
            getAuthor() >> name
            getTwitter() >> twitterHandler
        }
    }
}
