package com.jvm_bloggers.domain.query.published_newsletter_issue;

import com.jvm_bloggers.domain.query.NewsletterIssueNumber;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository;
import javaslang.control.Option;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
public class PublishedNewsletterIssueQuery {

    private final NewsletterIssueRepository newsletterIssueRepository;
    private final PublishedNewsletterIssueBuilder publishedNewsletterIssueBuilder;

    @Autowired
    public PublishedNewsletterIssueQuery(
        NewsletterIssueRepository newsletterIssueRepository,
        PublishedNewsletterIssueBuilder publishedNewsletterIssueBuilder) {
        this.newsletterIssueRepository = newsletterIssueRepository;
        this.publishedNewsletterIssueBuilder = publishedNewsletterIssueBuilder;
    }

    public Option<PublishedNewsletterIssue> getLatestIssue() {
        return newsletterIssueRepository
            .findFirstByOrderByPublishedDateDesc()
            .map(publishedNewsletterIssueBuilder::build);
    }

    public Option<PublishedNewsletterIssue> findByIssueNumber(NewsletterIssueNumber issueNumber) {
        return newsletterIssueRepository
                .findByIssueNumber(issueNumber.asLong())
                .map(publishedNewsletterIssueBuilder::build);
    }

}
