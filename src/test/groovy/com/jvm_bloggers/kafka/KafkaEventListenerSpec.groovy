package com.jvm_bloggers.kafka

import com.jvm_bloggers.core.newsletter_issues.NewIssuePublished
import com.jvm_bloggers.core.newsletter_issues.domain.NewsletterIssue
import com.jvm_bloggers.kafka.message.NewIssuePublishedMessage
import spock.lang.Specification
import spock.lang.Subject

class KafkaEventListenerSpec extends Specification {
    static final String ISSUE_URL = "http://jvm-bloggers.com/issue/"
    static final String TOPIC_NAME = "com.jvm_bloggers.issue.published"

    @Subject
    NewsletterPublishedListener eventListener
    MessagesPublisher messagesPublisher

    def setup() {
        messagesPublisher = Mock()
        eventListener = new NewsletterPublishedListener(messagesPublisher, ISSUE_URL, TOPIC_NAME)
    }


    def "should publish new Issue"() {
        given:
            def issueNumber = 15
            def issue = NewsletterIssue.builder().issueNumber(issueNumber).build()
            def event = new NewIssuePublished(issue)
            def expectedMsgPublished = "$ISSUE_URL" + issueNumber
        when:
            eventListener.publishNewIssue(event)
        then:
            1 * messagesPublisher.publish(new NewIssuePublishedMessage(issueNumber, expectedMsgPublished), TOPIC_NAME)
            0 * _
    }
}
