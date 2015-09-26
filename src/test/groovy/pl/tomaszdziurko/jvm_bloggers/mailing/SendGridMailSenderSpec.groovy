package pl.tomaszdziurko.jvm_bloggers.mailing

import com.sendgrid.SendGrid
import spock.lang.Specification
import spock.lang.Subject

class SendGridMailSenderSpec extends Specification {

    static String SENDER_ADDRESS = "example@example.com"

    SendGrid sendGrid = Mock(SendGrid)

    @Subject
    SendGridMailSender mailSender = new SendGridMailSender(sendGrid, SENDER_ADDRESS)

    def "Should send correct email"() {
        given:
            String recipient = "recipient"
            String subject = "Example subject"
            String htmlContent = "Example message"
            SendGrid.Email email = new SendGrid.Email()
            email.addTo(recipient)
            email.setFrom(SENDER_ADDRESS)
            email.setFromName(SendGridMailSender.FROM_NAME)
            email.setSubject(subject)
            email.setHtml(htmlContent)
        when:
            mailSender.sendEmail(recipient, subject, htmlContent)
        then:
            1 * sendGrid.send( {
                it.getTos() == [recipient] && it.getFrom() == SENDER_ADDRESS &&
                it.getFromName() == SendGridMailSender.FROM_NAME &&
                it.getSubject() == subject &&
                it.getHtml() == htmlContent
            }) >> new SendGrid.Response(1, "ok")
    }

}
