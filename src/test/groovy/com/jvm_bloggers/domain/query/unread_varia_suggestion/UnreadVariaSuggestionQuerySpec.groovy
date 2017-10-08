package com.jvm_bloggers.domain.query.unread_varia_suggestion

import com.jvm_bloggers.entities.varia_suggestion.VariaSuggestion
import com.jvm_bloggers.entities.varia_suggestion.VariaSuggestionRepository
import io.vavr.collection.List
import spock.lang.Specification
import spock.lang.Subject

class UnreadVariaSuggestionQuerySpec extends Specification {

    VariaSuggestionRepository variaSuggestionRepository = Stub()

    @Subject
    UnreadVariaSuggestionQuery unreadVariaSuggestionQuery = new UnreadVariaSuggestionQuery(variaSuggestionRepository)

    def "Should find unread VariaSuggestions"() {
        given:
        VariaSuggestion variaSuggestion = new VariaSuggestion('url', 'reason', 'author')

        when:
        variaSuggestionRepository.findByReadFalseOrReadNull(_) >> List.of(variaSuggestion)

        then:
        unreadVariaSuggestionQuery.findUnreadSuggestions(0, 10).size() == 1
    }
}
