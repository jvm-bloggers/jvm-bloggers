package pl.tomaszdziurko.jvm_bloggers.mailing;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.DEV;
import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.TEST;

@Component
@Profile({DEV, TEST})
@Slf4j
@Getter
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class LogMailSender implements MailSender {

    private final LogMailSenderPostAction logMailSenderPostAction;

    @Override
    public void sendEmail(String recipientAddress, String subject, String htmlContent) {
        log.debug("Sending email to '{}'\nSubject: {}\n{}", recipientAddress, subject, htmlContent);
        logMailSenderPostAction.postAction();
    }
}
