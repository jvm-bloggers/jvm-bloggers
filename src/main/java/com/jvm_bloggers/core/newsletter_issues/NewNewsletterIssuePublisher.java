package com.jvm_bloggers.core.newsletter_issues;

import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
class NewNewsletterIssuePublisher {

    private final NewsletterIssueFactory newsletterIssueFactory;
    private final NewsletterIssueRepository newsletterIssueRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Autowired
    public NewNewsletterIssuePublisher(
        NewsletterIssueFactory newsletterIssueFactory,
        NewsletterIssueRepository newsletterIssueRepository,
        ApplicationEventPublisher eventPublisher) {
        this.newsletterIssueFactory = newsletterIssueFactory;
        this.newsletterIssueRepository = newsletterIssueRepository;
        this.eventPublisher = eventPublisher;
    }

    @Transactional
    void publishNewIssue(int daysInThePastToIncludeInNewIssue) {
        NewsletterIssue newIssue = newsletterIssueFactory.create(daysInThePastToIncludeInNewIssue);
        log.info("Persisting new issue: {} ", newIssue);
        newsletterIssueRepository.save(newIssue);
        eventPublisher.publishEvent(new NewIssuePublished(newIssue));
    }

}
