package com.jvm_bloggers.core.mailing;

import com.jvm_bloggers.core.newsletter_issues.NewIssuePublished;
import com.jvm_bloggers.entities.email.Email;
import com.jvm_bloggers.entities.email.EmailRepository;
import com.jvm_bloggers.entities.mailing_address.MailingAddressRepository;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import com.jvm_bloggers.utils.DateTimeUtilities;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class EmailsWithNewIssueProducer {

    private static final String FROM_NAME = "JVM Bloggers";

    private static final String MAIL_SUMMARY_TITLE_PREFIX = "[JVM Bloggers] #";
    private static final String MAIL_SUMMARY_TITLE_POSTIFX = ": Nowe wpisy na polskich blogach, ";

    private final String senderAddress;
    private final MailingAddressRepository mailingAddressRepository;
    private final BlogSummaryMailGenerator mailContentGenerator;
    private final EmailRepository emailRepository;

    @Autowired
    public EmailsWithNewIssueProducer(@Value("${mailing.fromEmail}") String senderAddress,
                                      MailingAddressRepository mailingAddressRepository,
                                      BlogSummaryMailGenerator mailContentGenerator,
                                      EmailRepository emailRepository) {
        this.senderAddress = senderAddress;
        this.mailingAddressRepository = mailingAddressRepository;
        this.mailContentGenerator = mailContentGenerator;
        this.emailRepository = emailRepository;
    }

    @EventListener()
    public void handleNewIssueEvent(NewIssuePublished newIssuePublished) {
        log.info("Received event");
        NewsletterIssue newIssue = newIssuePublished.getNewsletterIssue();
        String emailTitle = prepareEmailTitle(newIssue);

        mailingAddressRepository.findAll().stream().forEach(mailingAddress ->
            saveEmailWithNewsletterIssue(newIssue, emailTitle, mailingAddress.getAddress())
        );
    }

    public void saveEmailWithNewsletterIssue(NewsletterIssue newIssue, String emailTitle,
                                             String recipient) {
        emailRepository.save(new Email(
            prepareSender(),
            recipient,
            emailTitle,
            prepareContent(newIssue)
        ));
    }

    private String prepareSender() {
        return FROM_NAME + " <" + senderAddress + ">";
    }

    private String prepareEmailTitle(NewsletterIssue newIssue) {
        return MAIL_SUMMARY_TITLE_PREFIX + newIssue.getIssueNumber() + MAIL_SUMMARY_TITLE_POSTIFX
            + newIssue.getPublishedDate().format(DateTimeUtilities.DATE_FORMATTER);
    }

    private String prepareContent(NewsletterIssue newIssue) {
        return mailContentGenerator.prepareMailContent(newIssue);
    }

}
