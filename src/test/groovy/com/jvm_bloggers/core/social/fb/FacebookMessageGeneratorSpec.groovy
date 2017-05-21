package com.jvm_bloggers.core.social.fb

import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue
import spock.lang.Specification

class FacebookMessageGeneratorSpec extends Specification {

    def "Should generate a Facebook message with an issue link"() {
        given:
            FacebookMessageGenerator generator = new FacebookMessageGenerator()
            String issueLink = "http://jvm-bloggers.com/issue/1"
            NewsletterIssue issue = new NewsletterIssue.NewsletterIssueBuilder()
                    .issueNumber(1L)
                    .heading("recent issue summary")
                    .build()

        when:
            String facebookMessage = generator.generateFacebookMessage(issueLink, issue)

        then:
            facebookMessage.contains(issueLink)
    }

    def "Should generate a Facebook message with an issue heading"() {
        given:
            FacebookMessageGenerator generator = new FacebookMessageGenerator()
            String issueLink = "http://jvm-bloggers.com/issue/1"
            String issueHeading = "recent issue summary"
            NewsletterIssue issue = new NewsletterIssue.NewsletterIssueBuilder()
                    .issueNumber(1L)
                    .heading(issueHeading)
                    .build()

        when:
            String facebookMessage = generator.generateFacebookMessage(issueLink, issue)

        then:
            facebookMessage.contains(issueHeading)
    }

}