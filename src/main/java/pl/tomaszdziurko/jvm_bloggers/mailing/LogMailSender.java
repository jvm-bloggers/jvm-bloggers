package pl.tomaszdziurko.jvm_bloggers.mailing;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

@Component
@Profile({"dev", "test"})
@Slf4j
public class LogMailSender implements MailSender {
    @Override
    public void sendEmail(String recipientAddress, String subject, String htmlContent) {
        log.debug("Sending email to '{}'\nSubject: {}\n{}", recipientAddress, subject, htmlContent);
    }
}
