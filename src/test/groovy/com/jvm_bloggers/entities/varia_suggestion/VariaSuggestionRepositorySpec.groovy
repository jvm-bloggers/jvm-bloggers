package com.jvm_bloggers.entities.varia_suggestion

import com.jvm_bloggers.SpringContextAwareSpecification
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.domain.PageRequest
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

    def "Should count unread VariaSuggestion"() {
        given:
        VariaSuggestion unread = new VariaSuggestion('url', 'reason', 'author')
        VariaSuggestion read = new VariaSuggestion('url1', 'reason1', 'author1')
        read.read = true

        when:
        variaSuggestionRepository.save(unread)
        variaSuggestionRepository.save(read)

        then:
        variaSuggestionRepository.count() == 2
        variaSuggestionRepository.countByReadFalseOrReadNull() == 1
    }

    def "Should find all unread VariaSuggestion"() {
        given:
        VariaSuggestion unread = new VariaSuggestion('url', 'reason', 'author')
        VariaSuggestion read = new VariaSuggestion('url1', 'reason1', 'author1')
        read.read = true

        when:
        variaSuggestionRepository.save(unread)
        variaSuggestionRepository.save(read)

        then:
        variaSuggestionRepository.count() == 2
        variaSuggestionRepository.findByReadFalseOrReadNull(new PageRequest(0, 10)).size() == 1
    }
}
