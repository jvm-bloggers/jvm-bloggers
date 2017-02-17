package com.jvm_bloggers.frontend.admin_area.mailing;


import com.jvm_bloggers.core.mailing.EmailsWithNewIssueProducer;
import com.jvm_bloggers.core.mailing.IssueNumberRetriever;
import com.jvm_bloggers.core.metadata.MetadataKeys;
import com.jvm_bloggers.core.metadata.MetadataRepository;
import com.jvm_bloggers.core.newsletter_issues.NewsletterIssueFactory;
import com.jvm_bloggers.core.newsletter_issues.domain.NewsletterIssue;
import com.jvm_bloggers.utils.DateTimeUtilities;
import com.jvm_bloggers.utils.NowProvider;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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
