package com.jvm_bloggers.frontend.public_area.newsletter_issue;

import com.jvm_bloggers.domain.query.NewsletterIssueNumber;
import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedNewsletterIssue;
import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedNewsletterIssueQuery;

import io.vavr.control.Option;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class NewsletterIssuePageBackingBean {

    private final PublishedNewsletterIssueQuery publishedNewsletterIssueQuery;

    @Autowired
    public NewsletterIssuePageBackingBean(PublishedNewsletterIssueQuery query) {
        this.publishedNewsletterIssueQuery = query;
    }

    public Option<PublishedNewsletterIssue> findByIssueNumber(NewsletterIssueNumber issueNumber) {
        return publishedNewsletterIssueQuery.findByIssueNumber(issueNumber);
    }

    public Option<NewsletterIssueNumber> findNextIssueNumber(NewsletterIssueNumber issueNumber) {
        return publishedNewsletterIssueQuery.findNextIssueNumber(issueNumber);
    }

    public Option<NewsletterIssueNumber> findPreviousIssueNumber(
        NewsletterIssueNumber issueNumber) {
        return publishedNewsletterIssueQuery.findPreviousIssueNumber(issueNumber);
    }

}
