package pl.tomaszdziurko.jvm_bloggers.mailing.sender;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.mailing.LogMailSenderPostAction;

import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.DEV;
import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.TEST;
import static pl.tomaszdziurko.jvm_bloggers.mailing.sender.MailSender.EmailSendingStatus.SUCCESS;

@Component
@Profile({DEV, TEST})
@Slf4j
@Getter
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class LogMailSender implements MailSender {

    private final LogMailSenderPostAction logMailSenderPostAction;

    @Override
    public EmailSendingStatus sendEmail(String fromAddress, String toAddress, String subject,
                                        String htmlContent) {
        log.debug("Sending email: from {}, to {}, title {}", fromAddress, toAddress, subject);
        logMailSenderPostAction.postAction();
        return SUCCESS;
    }
}
