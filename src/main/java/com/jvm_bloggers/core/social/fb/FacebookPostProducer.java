package com.jvm_bloggers.core.social.fb;

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator;
import com.jvm_bloggers.core.newsletter_issues.NewIssuePublished;
import com.jvm_bloggers.entities.fb.FacebookPost;
import com.jvm_bloggers.entities.fb.FacebookPostRepository;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
@Slf4j
class FacebookPostProducer {

    private final LinkGenerator linkGenerator;
    private final FacebookMessageGenerator messageGenerator;
    private final FacebookPostRepository facebookPostRepository;

    @Autowired
    FacebookPostProducer(LinkGenerator linkGenerator, FacebookMessageGenerator messageGenerator,
                         FacebookPostRepository facebookPostRepository) {
        this.linkGenerator = linkGenerator;
        this.messageGenerator = messageGenerator;
        this.facebookPostRepository = facebookPostRepository;
    }

    @EventListener()
    public void handleNewIssueEvent(NewIssuePublished newIssuePublished) {
        final NewsletterIssue issue = newIssuePublished.getNewsletterIssue();
        final String issueLink = linkGenerator.generateIssueLink(issue.getIssueNumber());
        final String facebookMessage = messageGenerator.generateFacebookMessage(issueLink, issue);
        FacebookPost post = new FacebookPost(issueLink, facebookMessage);
        facebookPostRepository.save(post);
    }

}
