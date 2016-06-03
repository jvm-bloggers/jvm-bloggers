package pl.tomaszdziurko.jvm_bloggers.mailing.sender;

import lombok.extern.slf4j.Slf4j;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.DEV;
import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.TEST;
import static pl.tomaszdziurko.jvm_bloggers.mailing.sender.MailSender.EmailSendingStatus.SUCCESS;

@Component
@Profile({DEV, TEST})
@Slf4j
public class LogMailSender implements MailSender {

    @Override
    public EmailSendingStatus sendEmail(String fromAddress, String toAddress, String subject,
        String htmlContent) {
        log.debug("Sending email: from {}, to {}, title {}", fromAddress, toAddress, subject);
        return SUCCESS;
    }
}
