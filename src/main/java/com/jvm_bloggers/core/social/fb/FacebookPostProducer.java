package com.jvm_bloggers.core.social.fb;

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator;
import com.jvm_bloggers.core.newsletter_issues.NewIssuePublished;
import com.jvm_bloggers.entities.fb.FacebookPost;
import com.jvm_bloggers.entities.fb.FacebookPostRepository;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import static lombok.AccessLevel.PACKAGE;

@Component
@Slf4j
@RequiredArgsConstructor(access = PACKAGE)
class FacebookPostProducer {

    private final LinkGenerator linkGenerator;
    private final FacebookMessageGenerator messageGenerator;
    private final FacebookPostRepository facebookPostRepository;

    @EventListener
    public void handleNewIssueEvent(NewIssuePublished newIssuePublished) {
        final NewsletterIssue issue = newIssuePublished.getNewsletterIssue();
        final String issueLink = linkGenerator.generateIssueLink(issue.getIssueNumber());
        final String facebookMessage = messageGenerator.generateFacebookMessage(issueLink);
        facebookPostRepository.save(new FacebookPost(issueLink, facebookMessage));
    }

}
