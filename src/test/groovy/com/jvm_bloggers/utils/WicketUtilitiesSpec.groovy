package com.jvm_bloggers.utils

import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

@Subject(WicketUtilities)
class WicketUtilitiesSpec extends Specification {

    @Unroll
    def 'Should calculate number of days since last friday for #date'() {
        when:
        def metaDataHeaderItem = WicketUtilities.forMetaTag("name", "value")

        then:
        metaDataHeaderItem.generateString() == """<meta name="name" content="value" property="name" />
"""
    }
}
