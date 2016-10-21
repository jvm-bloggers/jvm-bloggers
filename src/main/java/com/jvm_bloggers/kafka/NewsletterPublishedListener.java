package com.jvm_bloggers.kafka;

import com.jvm_bloggers.core.newsletter_issues.NewIssuePublished;
import com.jvm_bloggers.core.newsletter_issues.domain.NewsletterIssue;
import com.jvm_bloggers.kafka.message.NewIssuePublishedMessage;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

@Service
class NewsletterPublishedListener {

    private MessagesPublisher messagesPublisher;
    private final String issueUrl;
    private final String newIssuePublishedTopic;

    @Autowired
    public NewsletterPublishedListener(MessagesPublisher messagesPublisher,
                               @Value("${application.issueUrl:}") String issueUrl,
                               @Value("${kafka.topics.new.issue}") String newIssuePublishedTopic) {
        this.messagesPublisher = messagesPublisher;
        this.issueUrl = issueUrl;
        this.newIssuePublishedTopic = newIssuePublishedTopic;
    }

    @EventListener
    public void publishNewIssue(NewIssuePublished newIssuePublished) {
        NewsletterIssue issue = newIssuePublished.getNewsletterIssue();
        String url = issueUrl + issue.getIssueNumber();
        NewIssuePublishedMessage newIssuePublishedMessage = new NewIssuePublishedMessage(
                issue.getIssueNumber(), url);
        messagesPublisher.publish(newIssuePublishedMessage, newIssuePublishedTopic);
    }

}
