package com.jvm_bloggers.core.utils

import org.apache.wicket.extensions.markup.html.repeater.util.SortParam
import org.springframework.data.domain.Sort
import spock.lang.Specification
import spock.lang.Subject

@Subject(WicketToSpringSortingConverter)
class WicketToSpringSortingConverterSpec extends Specification {

    def "Should convert to spring sort testProperty asc"() {
        given:
            def param = new SortParam<String>("testProperty", true)

        when:
            def springSort = WicketToSpringSortingConverter.convert(param)

        then:
            Sort.by(Sort.Direction.ASC, "testProperty") == springSort.get()
    }

    def "Should convert to spring sort anotherTestProperty desc"() {
        given:
            def param = new SortParam<String>("anotherTestProperty", false)

        when:
            def springSort = WicketToSpringSortingConverter.convert(param)

        then:
            Sort.by(Sort.Direction.DESC, "anotherTestProperty") == springSort.get()
    }

    def "Should return empty Option"() {
        given:
            def param = null

        when:
            def springSort = WicketToSpringSortingConverter.convert(param)

        then:
            springSort.isEmpty()
    }
}
