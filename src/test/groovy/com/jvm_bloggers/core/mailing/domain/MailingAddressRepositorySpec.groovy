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

    def "Should detect existing address" (){
        given:
            MailingAddress mailingAddress = new MailingAddress(savedEmail)
            mailingAddressRepository.save(mailingAddress)
        when:
            boolean addressExists = mailingAddressRepository.addressExistsIgnoringId(testAddress, null)
        then:
            addressExists == expectedExistCheckResult
        where:
            savedEmail              | testAddress           | expectedExistCheckResult
            "example@example.pl"    | "example@example.pl"  | true
            "example@example.pl"    | "unique@example.pl"   | false
    }

    def "Should ignore address with given id during exist check" (){
        given:
            MailingAddress mailingAddress = new MailingAddress("example@example.pl")
            mailingAddressRepository.save(mailingAddress)
        expect:
            !mailingAddressRepository.addressExistsIgnoringId("example@example.pl", mailingAddress.id)
    }
}
