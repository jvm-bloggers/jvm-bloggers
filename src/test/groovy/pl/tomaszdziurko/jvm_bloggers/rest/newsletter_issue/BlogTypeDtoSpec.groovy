package pl.tomaszdziurko.jvm_bloggers.rest.newsletter_issue

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType
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
