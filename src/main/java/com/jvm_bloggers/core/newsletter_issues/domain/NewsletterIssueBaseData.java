package com.jvm_bloggers.core.newsletter_issues.domain;

import java.time.LocalDate;

public interface NewsletterIssueBaseData {

    Long getId();

    Long getIssueNumber();

    LocalDate getPublishedDate();
}
