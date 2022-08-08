package com.jvm_bloggers.validation

import com.networknt.schema.ValidationMessage
import com.networknt.schema.ValidatorTypeCode
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

@Subject(ValidatorMessageTranslator)
class ValidatorMessageTranslatorSpec extends Specification {

    @Unroll
    def "Should translate '#type' validator message"(final String path,
                                                     final ValidatorTypeCode type,
                                                     final String[] arguments,
                                                     final String expected) {
        given:
        def message = ValidationMessage.of(type.value, type, path, "schema", arguments)

        when:
        def result = ValidatorMessageTranslator.translate(Set.of(message))

        then:
        result == expected

        where:
        path         || type                           || arguments            || expected
        "\$.element" || ValidatorTypeCode.REQUIRED     || ["required_element"] || "Element '\$.element' does not contain required elements '[required_element]'!"
        "\$.element" || ValidatorTypeCode.PATTERN      || ["regexp_pattern"]   || "Element '\$.element' does not match the required pattern '[regexp_pattern]'!"
        "\$.element" || ValidatorTypeCode.MIN_LENGTH   || ["2"]                || "Element '\$.element' has an invalid 'minLength' length - the valid one is '[2]'!"
        "\$.element" || ValidatorTypeCode.MAX_LENGTH   || ["3"]                || "Element '\$.element' has an invalid 'maxLength' length - the valid one is '[3]'!"
        "\$.element" || ValidatorTypeCode.UNIQUE_ITEMS || ["required_element"] || "Array '\$.element' contains not unique elements!"
    }

    def "Should translate not supported message with a default message"() {
        given:
        def type = ValidatorTypeCode.MAX_ITEMS

        def message = ValidationMessage.of(type.value, type, "\$.element", "schema", "4")

        when:
        def result = ValidatorMessageTranslator.translate(Set.of(message))

        then:
        result == "Element '\$.element' violates 'maxItems' rule - 'there must be a maximum of 4 items in the array'"
    }

    def "Should translate several messages at once"() {
        given:
        def type1 = ValidatorTypeCode.MAX_ITEMS
        def type2 = ValidatorTypeCode.UNIQUE_ITEMS

        def message1 = ValidationMessage.of(type1.value, type1, "\$.element", "schema", "4")
        def message2 = ValidationMessage.of(type2.value, type2, "\$.array", "schema")

        when:
        def result = ValidatorMessageTranslator.translate(Set.of(message1, message2))

        then:
        result.contains("Element '\$.element' violates 'maxItems' rule - 'there must be a maximum of 4 items in the array'\n")
        result.contains("Array '\$.array' contains not unique elements!")
    }
}
