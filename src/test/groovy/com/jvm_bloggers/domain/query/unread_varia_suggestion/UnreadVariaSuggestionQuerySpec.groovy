package com.jvm_bloggers.domain.query.unread_varia_suggestion

import com.jvm_bloggers.entities.varia_suggestion.VariaSuggestion
import com.jvm_bloggers.entities.varia_suggestion.VariaSuggestionRepository
import io.vavr.collection.List
import spock.lang.Specification
import spock.lang.Subject

@Subject(UnreadVariaSuggestionQuery)
class UnreadVariaSuggestionQuerySpec extends Specification {

    def "Should find unread VariaSuggestions"() {
        given:
        VariaSuggestionRepository variaSuggestionRepository = Stub()
        UnreadVariaSuggestionQuery unreadVariaSuggestionQuery = new UnreadVariaSuggestionQuery(variaSuggestionRepository)

        when:
        variaSuggestionRepository.findByReadFalseOrReadNull(_) >> List.of(new VariaSuggestion('url', 'reason', 'author'))

        then:
        unreadVariaSuggestionQuery.findUnreadSuggestions(0, 10).size() == 1
    }
}
