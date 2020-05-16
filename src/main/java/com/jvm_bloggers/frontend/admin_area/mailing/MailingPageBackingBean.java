package com.jvm_bloggers.frontend.admin_area.mailing;

import com.jvm_bloggers.core.mailing.EmailsWithNewIssueProducer;
import com.jvm_bloggers.core.mailing.IssueNumberRetriever;
import com.jvm_bloggers.core.newsletter_issues.NewsletterIssueFactory;
import com.jvm_bloggers.domain.command.CommandPublisher;
import com.jvm_bloggers.domain.command.mark_varia_suggestion.MarkVariaSuggestion;
import com.jvm_bloggers.domain.command.save_metadata.SaveMetadata;
import com.jvm_bloggers.domain.query.MetadataQuery;
import com.jvm_bloggers.domain.query.unread_varia_suggestion.UnreadVariaSuggestion;
import com.jvm_bloggers.domain.query.unread_varia_suggestion.UnreadVariaSuggestionQuery;
import com.jvm_bloggers.entities.metadata.Metadata;
import com.jvm_bloggers.entities.metadata.MetadataKeys;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import com.jvm_bloggers.utils.DateTimeUtilities;
import com.jvm_bloggers.utils.NowProvider;
import io.vavr.collection.List;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@NoArgsConstructor
@AllArgsConstructor(onConstructor_ = {@Autowired})
public class MailingPageBackingBean {

    private CommandPublisher commandPublisher;
    private NowProvider nowProvider;
    private IssueNumberRetriever issueNumberRetriever;
    private NewsletterIssueFactory newsletterIssueFactory;
    private EmailsWithNewIssueProducer emailsWithNewIssueProducer;
    private MetadataQuery metadataQuery;
    private UnreadVariaSuggestionQuery unreadVariaSuggestionQuery;

    public String sendTestEmail() {
        int daysSinceLastFriday = DateTimeUtilities.daysBetweenDateAndLastFriday(nowProvider.now());
        NewsletterIssue newsletterIssue = newsletterIssueFactory.create(
            daysSinceLastFriday,
            issueNumberRetriever.getCurrentIssueNumber() + 1
        );

        String adminEmail = metadataQuery.findByName(MetadataKeys.ADMIN_EMAIL).getValue();
        emailsWithNewIssueProducer.saveEmailWithNewsletterIssue(
            newsletterIssue,
            "[JVM Bloggers] Test mail",
            adminEmail
        );

        return adminEmail;
    }

    public Metadata findMetadataByName(String name) {
        return metadataQuery.findByName(name);
    }

    public void saveMetadata(Metadata metadata) {
        commandPublisher.publish(new SaveMetadata(metadata));
    }

    public long countUnreadVariaSuggestion() {
        return unreadVariaSuggestionQuery.countUnread();
    }

    public List<UnreadVariaSuggestion> findUnreadSuggestions(int page, int size) {
        return unreadVariaSuggestionQuery.findUnreadSuggestions(page, size);
    }

    public void markVariaSuggestionAsRead(long variaSuggestionId) {
        commandPublisher.publish(new MarkVariaSuggestion(variaSuggestionId));
    }
}
