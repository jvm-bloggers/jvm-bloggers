package com.jvm_bloggers.domain.query.published_newsletter_issue;

import com.jvm_bloggers.domain.query.NewsletterIssueNumber;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository;

import io.vavr.control.Option;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.function.Function;

import static io.vavr.collection.Stream.of;

@Service
@Transactional(readOnly = true)
public class PublishedNewsletterIssueQuery {

    private final NewsletterIssueRepository newsletterIssueRepository;
    private final PublishedNewsletterIssueBuilder publishedNewsletterIssueBuilder;
    private Function<javaslang.control.Option<NewsletterIssue>, Option<NewsletterIssue>>
        javaslangToVavr =
            javaslang -> javaslang.map(Option::of).getOrElse(Option.none());

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
            .transform(javaslangToVavr)
            .map(publishedNewsletterIssueBuilder::build);
    }

    public Option<PublishedNewsletterIssue> findByIssueNumber(NewsletterIssueNumber issueNumber) {
        return newsletterIssueRepository
            .findByIssueNumber(issueNumber.asLong())
            .transform(javaslangToVavr)
            .map(publishedNewsletterIssueBuilder::build);
    }

    public Option<NewsletterIssueNumber> findNextIssueNumber(NewsletterIssueNumber issueNumber) {
        return of(issueNumber)
            .map(NewsletterIssueNumber::next)
            .find(this::issueNumberExist);
    }

    public Option<NewsletterIssueNumber> findPreviousIssueNumber(
        NewsletterIssueNumber issueNumber) {
        return of(issueNumber)
            .map(NewsletterIssueNumber::previous)
            .find(this::issueNumberExist);
    }

    private boolean issueNumberExist(NewsletterIssueNumber issueNumber) {
        return newsletterIssueRepository
            .existsByIssueNumber(issueNumber.asLong());
    }

}
