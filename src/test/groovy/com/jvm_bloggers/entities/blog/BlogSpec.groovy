package com.jvm_bloggers.entities.blog

import com.jvm_bloggers.utils.NowProvider
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

@Subject(Blog)
class BlogSpec extends Specification {

    @Unroll
    def "Should return approvedValue equal to #expectedInitialApprovedValue when moderationRequiredValue is #moderationRequiredValue"() {
        given:
        Blog blog = Blog.builder()
            .bookmarkableId("john-doe")
            .author("John Doe")
            .rss("http://john-doe.com/feed/")
            .url("url")
            .dateAdded(new NowProvider().now())
            .blogType(BlogType.PERSONAL)
            .moderationRequired(moderationRequiredValue)
            .build()

        when:
        Boolean approvedValue = blog.getInitialApprovedValue()

        then:
        approvedValue == expectedInitialApprovedValue

        where:
        moderationRequiredValue || expectedInitialApprovedValue
        true                    || null
        false                   || true
    }

}