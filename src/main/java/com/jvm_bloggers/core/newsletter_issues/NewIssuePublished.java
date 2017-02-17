package com.jvm_bloggers.core.newsletter_issues;

import com.jvm_bloggers.core.utils.JvmBloggersEvent;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@JvmBloggersEvent
@RequiredArgsConstructor
@Data
public class NewIssuePublished {

    private final NewsletterIssue newsletterIssue;

}
