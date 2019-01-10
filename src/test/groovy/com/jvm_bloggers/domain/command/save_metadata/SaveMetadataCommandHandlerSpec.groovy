package com.jvm_bloggers.domain.command.save_metadata

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.metadata.Metadata
import com.jvm_bloggers.entities.metadata.MetadataRepository
import org.springframework.beans.factory.annotation.Autowired
import spock.lang.Subject

@Subject(SaveMetadataCommandHandler)
class SaveMetadataCommandHandlerSpec extends SpringContextAwareSpecification {

    @Autowired
    SaveMetadataCommandHandler saveMetadataCommandHandler

    @Autowired
    MetadataRepository metadataRepository

    def "Should save Metadata through command"() {
        given:
        Metadata metadata = new Metadata(1L, 'name', 'value')

        when:
        saveMetadataCommandHandler.handle(new SaveMetadata(metadata))

        then:
        metadataRepository.findByName(metadata.name) == metadata
    }
}
