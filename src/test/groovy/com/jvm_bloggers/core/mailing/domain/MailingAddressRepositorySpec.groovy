package com.jvm_bloggers.core.mailing.domain

import org.springframework.beans.factory.annotation.Autowired
import com.jvm_bloggers.SpringContextAwareSpecification

class MailingAddressRepositorySpec extends SpringContextAwareSpecification {

    @Autowired
    MailingAddressRepository mailingAddressRepository

    def "Should persist MailingAddress"() {
        given:
            String email = "example@example.pl"
            MailingAddress address = new MailingAddress(email)
        when:
            mailingAddressRepository.save(address);
        then:
            MailingAddress savedAddress = mailingAddressRepository.findOne(address.id)
            savedAddress != null
            savedAddress.address == email
    }
}
