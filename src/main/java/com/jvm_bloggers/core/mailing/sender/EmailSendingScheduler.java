package com.jvm_bloggers.core.mailing.sender;


import com.jvm_bloggers.TimeConstants;
import com.jvm_bloggers.core.mailing.domain.Email;
import com.jvm_bloggers.core.mailing.domain.EmailRepository;
import com.jvm_bloggers.utils.NowProvider;

import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.Optional;

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

    @Scheduled(fixedDelay = TimeConstants.FOUR_MINUTES)
    public void sendOneEmail() {
        Optional<Email> notSentEmail = emailRepository.findFirstBySentDateNull();

        notSentEmail.ifPresent(email -> {
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
