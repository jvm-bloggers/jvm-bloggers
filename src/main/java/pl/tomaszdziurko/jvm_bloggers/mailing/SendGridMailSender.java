package pl.tomaszdziurko.jvm_bloggers.mailing;


import com.sendgrid.SendGrid;
import com.sendgrid.SendGridException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class SendGridMailSender {

    public static final String FROM_NAME = "JVM Bloggers";

    private final String sendGridApiKey;
    private final String senderAddress;

    @Autowired
    public SendGridMailSender(@Value("${sendgrid.apiKey}") String sendGridApiKey, @Value("${sendgrid.fromEmail}") String senderAddress) {
        this.sendGridApiKey = sendGridApiKey;
        this.senderAddress = senderAddress;
    }

    public void sendEmail(String recipientAddress, String subject, String htmlContent) {
        SendGrid sendgrid = new SendGrid(sendGridApiKey);
        SendGrid.Email email = prepareEmail(recipientAddress, subject, htmlContent);
        try {
            SendGrid.Response response = sendgrid.send(email);
            log.info(response.getMessage());
        } catch (SendGridException e) {
            log.error("Error when sending email to  " + recipientAddress + ", msg = " + e.getMessage(), e);
        }
    }

    private SendGrid.Email prepareEmail(String recipientAddress, String subject, String htmlContent) {
        SendGrid.Email email = new SendGrid.Email();
        email.addTo(recipientAddress);
        email.setFrom(senderAddress);
        email.setFromName(FROM_NAME);
        email.setSubject(subject);
        email.setHtml(htmlContent);
        return email;
    }
}
