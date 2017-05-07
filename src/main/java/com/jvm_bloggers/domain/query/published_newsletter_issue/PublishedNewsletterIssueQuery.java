package com.jvm_bloggers.domain.query.published_newsletter_issue;

import com.jvm_bloggers.domain.query.NewsletterIssueNumber;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository;
import javaslang.control.Option;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static javaslang.collection.Stream.of;

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

    public Option<NewsletterIssueNumber> findNextIssueNumber(NewsletterIssueNumber issueNumber) {
        return of(issueNumber.asLong())
            .map(NewsletterIssueNumber::next)
            .find(this::issueNumberExist);
    }

    public Option<NewsletterIssueNumber> findPreviousIssueNumber(
        NewsletterIssueNumber issueNumber) {
        return of(issueNumber.asLong())
            .map(NewsletterIssueNumber::previous)
            .find(this::issueNumberExist);
    }

    private boolean issueNumberExist(NewsletterIssueNumber issueNumber) {
        return newsletterIssueRepository
                .issueNumberExist(issueNumber.asLong());
    }

}
