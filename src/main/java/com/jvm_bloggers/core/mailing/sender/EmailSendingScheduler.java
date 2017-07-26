package com.jvm_bloggers.core.mailing.sender;

import com.jvm_bloggers.entities.email.Email;
import com.jvm_bloggers.entities.email.EmailRepository;
import com.jvm_bloggers.utils.NowProvider;
import io.vavr.control.Option;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class EmailSendingScheduler {

    private final EmailRepository emailRepository;
    private final MailSender mailSender;
    private final NowProvider nowProvider;

    @Autowired
    public EmailSendingScheduler(
        EmailRepository emailRepository,
        MailSender mailSender,
        NowProvider nowProvider
    ) {
        this.emailRepository = emailRepository;
        this.mailSender = mailSender;
        this.nowProvider = nowProvider;
    }

    @Scheduled(fixedDelayString = "${scheduler.send-email}")
    public void sendOneEmail() {
        Option<Email> notSentEmail = emailRepository.findFirstBySentDateNull();

        notSentEmail.forEach(email -> {
            MailSender.EmailSendingStatus status = mailSender.sendEmail(
                email.getFromAddress(),
                email.getToAddress(),
                email.getTitle(),
                email.getContent()
            );
            log.info("Mail sent to {}, status: {}", email.getToAddress(), status);
            setSentDateForSuccessfulAction(email, status);
        });
    }

    private void setSentDateForSuccessfulAction(Email email, MailSender.EmailSendingStatus status) {
        if (status.isOk()) {
            email.setSentDate(nowProvider.now());
            emailRepository.save(email);
        }
    }

}
