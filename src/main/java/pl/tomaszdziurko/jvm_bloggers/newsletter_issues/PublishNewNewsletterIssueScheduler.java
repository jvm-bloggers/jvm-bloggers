package pl.tomaszdziurko.jvm_bloggers.newsletter_issues;


import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class PublishNewNewsletterIssueScheduler {

    public static final int DAYS_IN_THE_PAST_TO_INCLUDE_IN_NEW_ISSUE = 7;
    private final NewNewsletterIssuePublisher newIssuePublisher;

    @Autowired
    public PublishNewNewsletterIssueScheduler(NewNewsletterIssuePublisher newIssuePublisher) {
        this.newIssuePublisher = newIssuePublisher;
    }

    //    @Scheduled(cron = TimeConstants.EVERY_FRIDAY_AT_12_OCLOCK)
    @Scheduled(initialDelay = 20 * 1000L, fixedDelay = 60 * 1000L)
    public void publishNewIssue() {
        log.info("Starting scheduler: generating new issue");
        newIssuePublisher.publishNewIssue(DAYS_IN_THE_PAST_TO_INCLUDE_IN_NEW_ISSUE);
    }
}
