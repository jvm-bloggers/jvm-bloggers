package com.jvm_bloggers.domain.query.newsletter_issue_for_listing;


import com.jvm_bloggers.domain.query.NewsletterIssueNumber;
import lombok.Value;

import java.time.LocalDate;

@Value
public class NewsletterIssueForListing {

    private NewsletterIssueNumber issueNumber;
    private LocalDate publicationDate;

}
