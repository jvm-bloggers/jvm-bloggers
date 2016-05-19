package pl.tomaszdziurko.jvm_bloggers.mailing.domain

import org.springframework.beans.factory.annotation.Autowired
import pl.tomaszdziurko.jvm_bloggers.SpringContextAwareSpecification

import java.time.LocalDateTime

class EmailRepositoryIntegrationSpec extends SpringContextAwareSpecification {

    @Autowired
    EmailRepository emailRepository

    def "Should persist Email entity"() {
        given:
            Email email = prepareEmail("some1@example.com")
        when:
            emailRepository.save(email)
        then:
            List<Email> allEmails = emailRepository.findAll()
            allEmails.size() == 1
            allEmails.get(0).id == email.id
    }

    def "Should find one not sent Email"() {
        given:
            Email email1 = prepareEmail("someA@example.com")
            Email email2 = prepareEmail("someB@example.com")
            email2.setSentDate(LocalDateTime.now())
            emailRepository.save(email1)
            emailRepository.save(email2)
        when:
            Optional<Email> notSentEmail = emailRepository.findOneBySentDateNull()
        then:
            notSentEmail.isPresent()
            notSentEmail.get().toAddress == email1.toAddress
    }

    def "Should find zero not sent emails "() {
        given:
            Email email1 = prepareEmail("someA@example.com")
            email1.setSentDate(LocalDateTime.now())
            Email email2 = prepareEmail("someB@example.com")
            email2.setSentDate(LocalDateTime.now())
            emailRepository.save(email1)
            emailRepository.save(email2)
        when:
            Optional<Email> notSentEmail = emailRepository.findOneBySentDateNull()
        then:
            !notSentEmail.isPresent()
    }

    private Email prepareEmail(String toAddress) {
        return new Email(
                "anyFromAddress",
                toAddress,
                "anyTitle",
                "anyContent"
        )
    }

}
