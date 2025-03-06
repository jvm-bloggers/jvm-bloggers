package com.jvm_bloggers.core.newsletter_issues;

import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository;
import com.jvm_bloggers.utils.NowProvider;
import java.time.LocalDate;
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
    private final NowProvider nowProvider;

    @Autowired
    public NewNewsletterIssuePublisher(
            NewsletterIssueFactory newsletterIssueFactory,
            NewsletterIssueRepository newsletterIssueRepository,
            ApplicationEventPublisher eventPublisher,
            NowProvider nowProvider) {
        this.newsletterIssueFactory = newsletterIssueFactory;
        this.newsletterIssueRepository = newsletterIssueRepository;
        this.eventPublisher = eventPublisher;
        this.nowProvider = nowProvider;
    }

    @Transactional
    void publishNewIssue(int daysInThePastToIncludeInNewIssue) {

        if (!todaysIssueExists()) {
            NewsletterIssue newIssue = newsletterIssueFactory.create(daysInThePastToIncludeInNewIssue);
            log.info("Persisting new issue: {} ", newIssue);
            newsletterIssueRepository.save(newIssue);
            eventPublisher.publishEvent(new NewIssuePublished(newIssue));
        }
    }

    private boolean todaysIssueExists() {
        LocalDate today = nowProvider.today();
        boolean todaysIssueExists = newsletterIssueRepository.existsByPublishedDate(today);
        log.info("Issue with publication date {} exists = " + todaysIssueExists, today);
        return todaysIssueExists;
    }

}
