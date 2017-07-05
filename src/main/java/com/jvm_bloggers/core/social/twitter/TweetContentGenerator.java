package com.jvm_bloggers.core.social.twitter;

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Random;

import static java.lang.String.format;
import static java.util.stream.Collectors.toList;
import static lombok.AccessLevel.PACKAGE;

@Component
@Slf4j
@RequiredArgsConstructor(access = PACKAGE)
class TweetContentGenerator {

    private static final String MESSAGE_TEMPLATE =
        "Nowy numer #%s już online - %s z postami m.in. %s i %s #java #jvm";

    private final LinkGenerator linkGenerator;
    private final Random random = new Random();

    public String generateTweetContent(NewsletterIssue issue) {
        final List<String> twitters =
            issue.getBlogPosts().stream().map(b -> b.getBlog().getTwitter()).collect(toList());
        final String tt1 = twitters.remove(random.nextInt(twitters.size()));
        final String tt2 = twitters.remove(random.nextInt(twitters.size()));
        final String issueLink = linkGenerator.generateIssueLink(issue.getIssueNumber());
        final String tweetContent = format(MESSAGE_TEMPLATE, issue.getIssueNumber(), issueLink, tt1, tt2);
        // check for length - link 23 chars, number 3 chars
        // two variants - with single mention and two of them
        // add at the end #java #jvm (higher priority than authors?)
        return tweetContent;
    }

}
