package pl.tomaszdziurko.jvm_bloggers.newsletter_issues;

import lombok.Data;
import lombok.RequiredArgsConstructor;

import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue;
import pl.tomaszdziurko.jvm_bloggers.utils.JvmBloggersEvent;

@JvmBloggersEvent
@RequiredArgsConstructor
@Data
public class NewIssuePublished {

    private final NewsletterIssue newsletterIssue;

}
