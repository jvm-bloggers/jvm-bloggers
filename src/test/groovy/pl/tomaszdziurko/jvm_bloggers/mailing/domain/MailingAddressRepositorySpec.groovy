package pl.tomaszdziurko.jvm_bloggers.mailing.domain

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.SpringApplicationContextLoader
import org.springframework.test.context.ContextConfiguration
import pl.tomaszdziurko.jvm_bloggers.JvmBloggersApplication
import pl.tomaszdziurko.jvm_bloggers.SpringContextAwareSpecification;
import spock.lang.Specification


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
