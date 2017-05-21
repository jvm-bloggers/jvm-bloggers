package com.jvm_bloggers.core.social.fb;

import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import org.springframework.stereotype.Component;

@Component
class FacebookMessageGenerator {

    String generateFacebookMessage(String issueLink, NewsletterIssue issue) {
        return issueLink + " " + issue.getHeading(); // TODO
    }

}
