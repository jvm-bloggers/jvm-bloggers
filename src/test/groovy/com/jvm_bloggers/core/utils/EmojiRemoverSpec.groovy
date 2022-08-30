package com.jvm_bloggers.core.utils

import spock.lang.Specification
import spock.lang.Subject

@Subject(EmojiRemover)
class EmojiRemoverSpec extends Specification {

    private def remover = new EmojiRemover()

    def "Should remove emojis from the given literal"() {
        given:
        def input = "An 😀awesome 😃blog post's title with a few 😉emojis!";

        when:
        def result = remover.remove(input)

        then:
        result == "An awesome blog post's title with a few emojis!"
    }

    def "Should return empty string when the input literal is null or blank"(String input) {
        when:
        def result = remover.remove(input)

        then:
        result == ""

        where:
        input << ["", null, "     "]
    }
}
