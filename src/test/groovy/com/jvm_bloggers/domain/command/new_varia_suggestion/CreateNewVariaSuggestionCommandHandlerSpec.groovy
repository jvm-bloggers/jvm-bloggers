package com.jvm_bloggers.domain.command.new_varia_suggestion

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.domain.command.CommandPublisher
import com.jvm_bloggers.entities.varia_suggestion.VariaSuggestionRepository
import org.springframework.beans.factory.annotation.Autowired
import spock.lang.Subject

@Subject(CreateNewVariaSuggestionCommandHandler)
class CreateNewVariaSuggestionCommandHandlerSpec extends SpringContextAwareSpecification {

    @Autowired
    CommandPublisher commandPublisher

    @Autowired
    VariaSuggestionRepository variaSuggestionRepository

    def "Should create new varia suggestion"() {
        given:
        CreateNewVariaSuggestion command = new CreateNewVariaSuggestion(
                'url',
                'reason',
                'author'
        )

        when:
        commandPublisher.publish(command)

        then:
        variaSuggestionRepository.count() == 1
    }
}
