package com.jvm_bloggers.utils

import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

@Subject(CleanString)
class CleanStringSpec extends Specification {

    @Unroll
    def 'Should not lower case #input'(String input) {
        when:
        String result = CleanString.clean(input)

        then:
        result == input

        where:
        input << titlesNotExceedingUpperCaseLimit()
    }

    @Unroll
    def 'Should lower case #input'(String input, String expected) {
        when:
        String result = CleanString.clean(input)

        then:
        result == expected

        where:
        input                                               | expected
        "THIS IS SO CATCHY VERY interesting blog Post title"| "This is so catchy very interesting blog post title"
        "THIS IS SO ĄĘŚĆŁŻ VERY interesting blog Post title"| "This is so ąęśćłż very interesting blog post title"
        "JUNIT 4"                                           | "Junit 4"
    }

    private static List<String> titlesNotExceedingUpperCaseLimit() {
        return Arrays.asList("",
        "THIS is VERY interestin blog\npost title 20 percent",
        "THIS is VERY interestin\tblog post ąęśćł 20 percent",
        "abc defgh",
        "123-456-789 \$% 09\n",
        "A 123 456",
        //some real world examples
        "WJUG #274 Piotr Przybył - \"Java 15: Nowości warte uwagi\"",
        "O prawie Demeter, Clean Code i zasadach SOLID z Piotrem Stawirejem",
        "Czy zmiany w JPK-V7 wpłyną na rozliczanie IP BOX",
        "SSH like a Pro")
    }
}