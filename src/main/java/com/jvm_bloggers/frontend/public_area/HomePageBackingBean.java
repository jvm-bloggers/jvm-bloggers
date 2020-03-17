package com.jvm_bloggers.frontend.public_area;

import com.jvm_bloggers.domain.query.NewsletterIssueNumber;
import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedNewsletterIssue;
import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedNewsletterIssueQuery;

import io.vavr.control.Option;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class HomePageBackingBean {

    private final PublishedNewsletterIssueQuery publishedNewsletterIssueQuery;

    @Autowired
    public HomePageBackingBean(PublishedNewsletterIssueQuery publishedNewsletterIssueQuery) {
        this.publishedNewsletterIssueQuery = publishedNewsletterIssueQuery;
    }

    public Option<PublishedNewsletterIssue> getLatestIssue() {
        return publishedNewsletterIssueQuery.getLatestIssue();
    }

    public Option<NewsletterIssueNumber> findNextIssueNumber(NewsletterIssueNumber issueNumber) {
        return publishedNewsletterIssueQuery.findNextIssueNumber(issueNumber);
    }

    public Option<NewsletterIssueNumber> findPreviousIssueNumber(NewsletterIssueNumber issueNumber) {
        return publishedNewsletterIssueQuery.findPreviousIssueNumber(issueNumber);
    }
}
