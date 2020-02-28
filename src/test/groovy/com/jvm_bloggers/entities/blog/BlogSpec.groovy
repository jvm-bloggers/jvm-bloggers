package com.jvm_bloggers.entities.blog

import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import static com.jvm_bloggers.ObjectMother.aBlog

@Subject(Blog)
class BlogSpec extends Specification {

    @Unroll
    def "Should return approvedValue equal to #expectedInitialApprovedValue when moderationRequiredValue is #moderationRequiredValue"() {
        given:
        Blog blog = aBlog(moderationRequired: moderationRequiredValue)

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