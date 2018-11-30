package com.jvm_bloggers.utils

import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

@Subject(HtmlEmptinessChecker)
class HtmlEmptinessCheckerSpec extends Specification {

    @Unroll
    def "Should properly check for section isNotEmpty() result when content = `#content`"() {

        when:
        boolean isNotEmpty = HtmlEmptinessChecker.isNotEmpty(content)

        then:
        isNotEmpty == expectedValue

        where:
        content                           || expectedValue
        ''                                || false
        '  '                              || false
        null                              || false
        '<br>'                            || false
        '<div><br></div>'                 || false
        '&nbsp;'                          || false
        '<br/>&nbsp;'                     || false
        ' - content'                      || true
        '<div>content</div>'              || true
        '<br>content<div>something</div>' || true
        '&nbsp; - item'                   || true
    }

    @Unroll
    def "Should properly check for section isEmpty() result when content = `#content`"() {

        when:
        boolean isEmpty = HtmlEmptinessChecker.isEmpty(content)

        then:
        isEmpty == expectedValue

        where:
        content                           || expectedValue
        ''                                || true
        '  '                              || true
        null                              || true
        '<br>'                            || true
        '<div><br></div>'                 || true
        '&nbsp;'                          || true
        '<br/>&nbsp;'                     || true
        ' - content'                      || false
        '<div>content</div>'              || false
        '<br>content<div>something</div>' || false
        '&nbsp; - item'                   || false
    }

}
