package pl.tomaszdziurko.jvm_bloggers.newsletter_issues;

import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue;
import pl.tomaszdziurko.jvm_bloggers.utils.JvmBloggersEvent;

@JvmBloggersEvent
public class NewIssuePublished {

    private final NewsletterIssue newsletterIssue;

    public NewIssuePublished(NewsletterIssue newsletterIssue) {
        this.newsletterIssue = newsletterIssue;
    }

    public NewsletterIssue getNewsletterIssue() {
        return newsletterIssue;
    }
}
