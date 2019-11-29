package com.jvm_bloggers.domain.command.mark_varia_suggestion;

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.domain.command.CommandPublisher
import com.jvm_bloggers.entities.varia_suggestion.VariaSuggestion
import com.jvm_bloggers.entities.varia_suggestion.VariaSuggestionRepository
import org.springframework.beans.factory.annotation.Autowired
import spock.lang.Subject;

@Subject(MarkVariaSuggestionCommandHandler)
class MarkVariaSuggestionCommandHandlerSpec extends SpringContextAwareSpecification {

    @Autowired
    CommandPublisher commandPublisher

    @Autowired
    VariaSuggestionRepository variaSuggestionRepository

    def "Should mark VariaSuggestion as read"() {
        given:
        Long id = variaSuggestionRepository.save(new VariaSuggestion('url', 'reason', 'author')).id

        when:
        commandPublisher.publish(new MarkVariaSuggestion(id))

        then:
        variaSuggestionRepository.findById(id).get().read
    }
}
