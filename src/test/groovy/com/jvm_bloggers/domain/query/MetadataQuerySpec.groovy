package com.jvm_bloggers.domain.query

import com.jvm_bloggers.entities.metadata.Metadata
import com.jvm_bloggers.entities.metadata.MetadataRepository
import spock.lang.Specification
import spock.lang.Subject

class MetadataQuerySpec extends Specification {

    MetadataRepository metadataRepository = Stub(MetadataRepository)

    @Subject
    MetadataQuery metadataQuery = new MetadataQuery(metadataRepository)

    def "Should find Metadata by name"() {
        given:
        String name = 'name'
        Metadata metadata = new Metadata(1L, name, 'value')
        metadataRepository.findByName(name) >> metadata

        when:
        Metadata result = metadataQuery.findByName(name)

        then:
        result == metadata
    }
}
