package pl.tomaszdziurko.jvm_bloggers.rest.newsletter_issue

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType
import pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue.BlogDto
import pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue.BlogTypeDto
import spock.lang.Specification

import static java.time.LocalDateTime.now

class BlogDtoSpec extends Specification {

    def "Should convert blog to its DTO representation"() {
        given:
            Blog blog = new Blog(1, 2, "some author", "some rss", "some url",
                    "some twitter", now(), BlogType.PERSONAL, true)
        when:
            BlogDto blogJson = BlogDto.fromBlog(blog)
        then:
            blogJson.author == blog.getAuthor()
            blogJson.url == blog.getUrl()
            blogJson.authorTwitterHandle == blog.getTwitter()
            blogJson.type == BlogTypeDto.fromBlogType(blog.getBlogType())
    }

}
