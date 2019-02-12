package com.jvm_bloggers.utils

import com.jvm_bloggers.core.utils.Validators
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

@Subject(Validators)
class ValidatorsSpec extends Specification {

    @Unroll
    def "Should validate url #url as valid = #expectedValidationResult"() {
        when:
        boolean isValid = Validators.isUrlValid(url)

        then:
        isValid == expectedValidationResult

        where:
        url                                                                            || expectedValidationResult
        "https://google.com/something"                                                 || true
        "https://softwaremill.com/2015-in-numbers/"                                    || true
        "http://overtone-recipes.github.io//remake/2016/04/03/recreating-da-funk.html" || true
        "https://softwaremill.com//2015-in-numbers/"                                   || true
        "ftp://some-ftp-server.com"                                                    || false
        "//relative-address.pl/some-blog-post"                                         || false
        ""                                                                             || false
        "invalid"                                                                      || false
        "9802389023802934098234"                                                       || false
        null                                                                           || false
    }
}
