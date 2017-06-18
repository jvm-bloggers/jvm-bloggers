package com.jvm_bloggers.utils

import spock.lang.Specification
import spock.lang.Unroll

class HtmlEmptinessCheckerSpec extends Specification {

    @Unroll
    def "Should properly check for section emptiness when content = `#content`"() {

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

}
