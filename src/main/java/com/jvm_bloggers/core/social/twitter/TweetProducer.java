package com.jvm_bloggers.core.social.twitter;

import com.jvm_bloggers.core.data_fetching.blogs.NewBlogAdded;
import com.jvm_bloggers.core.newsletter_issues.NewIssuePublished;
import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import com.jvm_bloggers.entities.twitter.Tweet;
import com.jvm_bloggers.entities.twitter.TweetRepository;

import com.jvm_bloggers.utils.DateTimeUtilities;
import com.jvm_bloggers.utils.NowProvider;
import com.jvm_bloggers.utils.TimeRange;
import io.vavr.collection.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;

import static lombok.AccessLevel.PACKAGE;

@Component
@Slf4j
@RequiredArgsConstructor(access = PACKAGE)
class TweetProducer {

    private static final TimeRange ALLOWED_PUBLICATION_TIME_RANGE =
            TimeRange.between(LocalTime.of(8, 0), LocalTime.of(23, 59));

    private static final int MIN_HOURS_DIFF = 8;
    private static final int MAX_HOURS_DIFF = 12;

    private final TweetContentGenerator contentGenerator;
    private final TweetRepository tweetRepository;
    private final NowProvider nowProvider;

    @EventListener
    public void handleNewIssueEvent(NewIssuePublished newIssuePublished) {
        final NewsletterIssue issue = newIssuePublished.getNewsletterIssue();
        final String content = contentGenerator.generateTweetContent(issue);
        tweetRepository.save(new Tweet(content, nowProvider.now()));
    }

    @EventListener
    public void handleNewBlogAddedEvent(NewBlogAdded newBlogAdded) {
        Blog newBlog = newBlogAdded.getNewBlog();
        NewBlogTweetContents contents = contentGenerator.generateTweetContent(newBlog);
        scheduleTweets(contents);
    }

    private void scheduleTweets(NewBlogTweetContents content) {
        LocalDateTime now = nowProvider.now();
        LocalDateTime nextPublicationDate = calculateNextPublicationDate(now);
        Tweet tweetForNow = new Tweet(content.getFirstTweetContent(), now);
        Tweet futureTweet = new Tweet(content.getSecondTweetContent(), nextPublicationDate);
        tweetRepository.saveAll(List.of(tweetForNow, futureTweet));
    }

    private LocalDateTime calculateNextPublicationDate(LocalDateTime startTime) {
        LocalDateTime nextPublicationDate = randomizePublicationDate(startTime);
        if (ALLOWED_PUBLICATION_TIME_RANGE.contains(nextPublicationDate.toLocalTime())) {
            return nextPublicationDate;
        }
        return nextPublicationDate.withHour(ALLOWED_PUBLICATION_TIME_RANGE.getStartTime().getHour());
    }

    private LocalDateTime randomizePublicationDate(LocalDateTime startTime) {
        return DateTimeUtilities.randomDateTimeStartingFrom(startTime, MIN_HOURS_DIFF, MAX_HOURS_DIFF, ChronoUnit.HOURS);
    }
}
