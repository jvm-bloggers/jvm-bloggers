package com.jvm_bloggers.core.social.twitter;

import com.jvm_bloggers.core.newsletter_issues.NewIssuePublished;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import com.jvm_bloggers.entities.twitter.Tweet;
import com.jvm_bloggers.entities.twitter.TweetRepository;

import com.jvm_bloggers.utils.NowProvider;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import static lombok.AccessLevel.PACKAGE;

@Component
@Slf4j
@RequiredArgsConstructor(access = PACKAGE)
class TweetProducer {

    private final TweetContentGenerator contentGenerator;
    private final TweetRepository tweetRepository;
    private final NowProvider nowProvider;

    @EventListener
    public void handleNewIssueEvent(NewIssuePublished newIssuePublished) {
        final NewsletterIssue issue = newIssuePublished.getNewsletterIssue();
        final String content = contentGenerator.generateTweetContent(issue);
        tweetRepository.save(new Tweet(content, nowProvider.now()));
    }

}
