package com.jvm_bloggers.core.social.fb;

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator;
import com.jvm_bloggers.core.newsletter_issues.NewIssuePublished;
import com.jvm_bloggers.domain.command.top_posts_summary.TopPostsSummaryGenerated;
import com.jvm_bloggers.entities.fb.FacebookPost;
import com.jvm_bloggers.entities.fb.FacebookPostRepository;
import com.jvm_bloggers.utils.NowProvider;
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
    private final NowProvider nowProvider;

    @EventListener
    public void handleNewIssueEvent(NewIssuePublished newIssuePublished) {
        final String issueLink = linkGenerator.generateIssueLink(
                newIssuePublished.getNewsletterIssue().getIssueNumber());
        facebookPostRepository.save(
            new FacebookPost(
                issueLink,
                messageGenerator.generateFacebookMessage(issueLink,
                                FacebookMessageTemplate.NEW_ISSUE_AVAILABLE),
                nowProvider.now()
            )
        );
    }

    @EventListener
    public void handleTopPostSummaryGeneratedEvent(
            TopPostsSummaryGenerated topPostsSummaryGenerated) {
        final String link = linkGenerator
                .generateTopPostsSummaryLink(topPostsSummaryGenerated.getYearMonth());
        facebookPostRepository.save(
                new FacebookPost(
                        link,
                        messageGenerator.generateFacebookMessage(link,
                                        FacebookMessageTemplate.TOP_POSTS_SUMMARY),
                                        nowProvider.now()
                )
        );
    }
}
