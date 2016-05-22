package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;


import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.mailing.EmailsWithNewIssueProducer;
import pl.tomaszdziurko.jvm_bloggers.mailing.IssueNumberRetriever;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataKeys;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataRepository;
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.NewsletterIssueFactory;
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue;
import pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

@Component
@NoArgsConstructor
@AllArgsConstructor(onConstructor = @__(@Autowired))
public class MailingPageRequestHandler {

    private NowProvider nowProvider;
    private IssueNumberRetriever issueNumberRetriever;
    private NewsletterIssueFactory newsletterIssueFactory;
    private EmailsWithNewIssueProducer emailsWithNewIssueProducer;
    private MetadataRepository metadataRepository;

    public String sendTestEmail() {
        int daysSinceLastFriday = DateTimeUtilities.daysBetweenDateAndLastFriday(nowProvider.now());
        NewsletterIssue newsletterIssue = newsletterIssueFactory.create(
            daysSinceLastFriday,
            issueNumberRetriever.getCurrentIssueNumber() + 1
        );

        String adminEmail = metadataRepository.findByName(MetadataKeys.ADMIN_EMAIL).getValue();
        emailsWithNewIssueProducer.saveEmailWithNewsletterIssue(
            newsletterIssue,
            "[JVM Bloggers] Test mail",
            adminEmail
        );

        return adminEmail;
    }

}
