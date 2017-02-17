package com.jvm_bloggers.frontend.public_area.newsletter_issue

import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogType
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
