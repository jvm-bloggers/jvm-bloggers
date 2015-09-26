package pl.tomaszdziurko.jvm_bloggers.mailing.domain

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.SpringApplicationContextLoader
import org.springframework.test.context.ContextConfiguration
import pl.tomaszdziurko.jvm_bloggers.JvmBloggersApplication
import spock.lang.Specification

@ContextConfiguration(classes = [JvmBloggersApplication], loader = SpringApplicationContextLoader)
class MailingAddressRepositorySpec extends Specification {

    @Autowired
    MailingAddressRepository mailingAddressRepository

    def setupSpec() {
        System.setProperty("jasypt.encryptor.password", "password")
    }

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
