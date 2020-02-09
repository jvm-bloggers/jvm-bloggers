package com.jvm_bloggers.domain.query.published_newsletter_issue

import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogType
import spock.lang.Specification
import spock.lang.Subject

import static java.time.LocalDateTime.now
import static java.lang.Boolean.TRUE

@Subject(NewlyAddedBlog)
class NewlyAddedBlogSpec extends Specification {

    def "Should convert blog to its DTO representation"() {
        given:
        Blog blog = new Blog(1,"someCode","some author", "some rss", "some url",
            "some twitter", now(), BlogType.PERSONAL, true, TRUE)

        when:
        NewlyAddedBlog blogJson = NewlyAddedBlog.fromBlog(blog)

        then:
        blogJson.author == blog.getAuthor()
        blogJson.url == blog.getUrl()
        blogJson.authorTwitterHandle == blog.getTwitter()
    }

}
