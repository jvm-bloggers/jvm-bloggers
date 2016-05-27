package pl.tomaszdziurko.jvm_bloggers.newsletter_issues;


import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue;
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssueRepository;

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
        ApplicationEventPublisher eventPublisher
    ) {
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
