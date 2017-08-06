package com.jvm_bloggers.core.social.twitter;

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator;
import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;

import io.vavr.collection.List;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.stereotype.Component;

import static java.lang.String.format;
import static lombok.AccessLevel.PACKAGE;

@Component
@Slf4j
@RequiredArgsConstructor(access = PACKAGE)
class TweetContentGenerator {

    private static final int TWEET_MAX_LENGTH = 140;
    private static final String MESSAGE_TEMPLATE =
        "Nowy numer #%s już online - %s z postami m.in. %s, %s i %s #java #jvm";
    private static final String SHORT_MESSAGE_TEMPLATE =
        "Nowy numer #%s już online - %s z postami m.in. %s i %s #java #jvm";

    private final LinkGenerator linkGenerator;

    public String generateTweetContent(NewsletterIssue issue) {
        final List<String> personalTTs =
            List.ofAll(issue.getBlogPosts())
                .map(BlogPost::getBlog)
                .filter(Blog::isPersonal)
                .map(Blog::getTwitter)
                .shuffle()
                .take(2);

        final String companyTT =
            List.ofAll(issue.getBlogPosts())
                .map(BlogPost::getBlog)
                .filter(b -> !b.isPersonal())
                .map(Blog::getTwitter)
                .shuffle()
                .head();

        final String issueLink = linkGenerator.generateIssueLink(issue.getIssueNumber());
        final String tweetContent =
            format(
                MESSAGE_TEMPLATE, issue.getIssueNumber(), issueLink,
                personalTTs.get(0), companyTT, personalTTs.get(1)
            );

        if (tweetIsTooLong(tweetContent, issueLink.length())) {
            return format(
                SHORT_MESSAGE_TEMPLATE, issue.getIssueNumber(), issueLink,
                personalTTs.get(0), companyTT
            );
        } else {
            return tweetContent;
        }
    }

    private boolean tweetIsTooLong(String tweetContent, int originalIssuesLinkLength) {
        return (tweetContent.length() - originalIssuesLinkLength + 23) > TWEET_MAX_LENGTH;
    }

}
