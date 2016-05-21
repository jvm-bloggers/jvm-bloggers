package pl.tomaszdziurko.jvm_bloggers.mailing;

import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.mailing.domain.Email;
import pl.tomaszdziurko.jvm_bloggers.mailing.domain.EmailRepository;
import pl.tomaszdziurko.jvm_bloggers.mailing.domain.MailingAddressRepository;
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.NewIssuePublished;
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue;

import java.time.format.DateTimeFormatter;

@Component
@Slf4j
public class EmailsWithNewIssueProducer {

    private static final String FROM_NAME = "JVM Bloggers";

    public static final String MAIL_SUMMARY_TITLE_PREFIX = "[JVM Bloggers] #";
    public static final String MAIL_SUMMARY_TITLE_POSTIFX = ": Nowe wpisy na polskich blogach, ";
    public static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy");

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

        mailingAddressRepository.findAll().stream().forEach(mailingAddress -> {
            saveEmailWithNewsletterIssue(newIssue, emailTitle, mailingAddress.getAddress());
        });
    }

    public void saveEmailWithNewsletterIssue(NewsletterIssue newIssue, String emailTitle,
                                             String recipient) {
        Email email = new Email(
            prepareSender(),
            recipient,
            emailTitle,
            prepareContent(newIssue)
        );
        emailRepository.save(email);
    }

    private String prepareSender() {
        return FROM_NAME + " <" + senderAddress + ">";
    }

    private String prepareEmailTitle(NewsletterIssue newIssue) {
        return MAIL_SUMMARY_TITLE_PREFIX + newIssue.getIssueNumber() + MAIL_SUMMARY_TITLE_POSTIFX
            + newIssue.getPublishedDate().format(FORMATTER);
    }

    private String prepareContent(NewsletterIssue newIssue) {
        return mailContentGenerator.prepareMailContent(newIssue);
    }

}
