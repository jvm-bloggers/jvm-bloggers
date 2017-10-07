package com.jvm_bloggers.entities.varia_suggestion

import com.jvm_bloggers.SpringContextAwareSpecification
import org.springframework.beans.factory.annotation.Autowired
import spock.lang.Subject

class VariaSuggestionRepositorySpec extends SpringContextAwareSpecification {

    @Subject
    @Autowired
    VariaSuggestionRepository variaSuggestionRepository

    def "Should persist VariaSuggestion entity"() {
        given:
        VariaSuggestion variaSuggestion = new VariaSuggestion(
                'url',
                'reason',
                'author'
        )

        when:
        variaSuggestionRepository.save(variaSuggestion)

        then:
        List<VariaSuggestion> allSuggestions = variaSuggestionRepository.findAll()
        allSuggestions.size() == 1
        allSuggestions.get(0).createDate != null
    }
}
