package com.jvm_bloggers.core.social.twitter.publisher;

import com.jvm_bloggers.core.social.twitter.publisher.TwitterPublisher.TwitterPublishingStatus;
import com.jvm_bloggers.entities.twitter.Tweet;
import com.jvm_bloggers.entities.twitter.TweetRepository;
import com.jvm_bloggers.utils.NowProvider;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import io.vavr.control.Option;

import static lombok.AccessLevel.PACKAGE;

@Component
@Slf4j
@RequiredArgsConstructor(access = PACKAGE)
class TwitterPublishingScheduler {

    private final TweetRepository tweetRepository;
    private final TwitterPublisher twitterPublisher;
    private final NowProvider nowProvider;

    @Scheduled(fixedDelayString = "${scheduler.publish-twitter}")
    public void publishOnePost() {
        Option<Tweet> notSentTweet = tweetRepository.findFirstBySentDateNull();

        notSentTweet.forEach(tweet -> {
            final TwitterPublishingStatus status = twitterPublisher.publish(tweet);
            log.info("Tweet published, status: {}", status);
            setSentDateForSuccessfulAction(tweet, status);
        });
    }

    private void setSentDateForSuccessfulAction(
        Tweet tweet,
        TwitterPublishingStatus status) {
        if (status.isOk()) {
            tweet.setSentDate(nowProvider.now());
            tweetRepository.save(tweet);
        }
    }

}
