package com.jvm_bloggers.frontend.public_area.newsletter_issue

import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogType
import spock.lang.Specification
import spock.lang.Unroll

class BlogTypeDtoSpec extends Specification {

    @Unroll
    def "Should map #blogType into some BlogTypeDto value"() {
        when:
            BlogTypeDto type = BlogTypeDto.fromBlogType(blogType)
        then:
            type != null
        where:
            blogType << BlogType.values()
    }
}
