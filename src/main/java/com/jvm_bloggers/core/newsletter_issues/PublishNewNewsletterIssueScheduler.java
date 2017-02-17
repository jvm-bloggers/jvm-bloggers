package com.jvm_bloggers.core.newsletter_issues;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class PublishNewNewsletterIssueScheduler {

    private static final int DAYS_IN_THE_PAST_TO_INCLUDE_IN_NEW_ISSUE = 7;

    private final NewNewsletterIssuePublisher newIssuePublisher;

    @Autowired
    public PublishNewNewsletterIssueScheduler(NewNewsletterIssuePublisher newIssuePublisher) {
        this.newIssuePublisher = newIssuePublisher;
    }

    @Scheduled(cron = "${scheduler.publish-new-issue}")
    public void publishNewIssue() {
        log.info("Starting scheduler: generating new issue");
        newIssuePublisher.publishNewIssue(DAYS_IN_THE_PAST_TO_INCLUDE_IN_NEW_ISSUE);
    }
}
