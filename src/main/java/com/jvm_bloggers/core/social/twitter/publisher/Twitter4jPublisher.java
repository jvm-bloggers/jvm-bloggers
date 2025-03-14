package com.jvm_bloggers.core.social.twitter.publisher;

import com.jvm_bloggers.entities.twitter.Tweet;

import io.vavr.control.Try;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import twitter4j.v1.TwitterV1;

import static com.jvm_bloggers.ApplicationProfiles.PRODUCTION;
import static com.jvm_bloggers.core.social.twitter.publisher.TwitterPublisher.TwitterPublishingStatus.ERROR;
import static com.jvm_bloggers.core.social.twitter.publisher.TwitterPublisher.TwitterPublishingStatus.SUCCESS;
import static lombok.AccessLevel.PACKAGE;

@RequiredArgsConstructor(access = PACKAGE)
@Slf4j
class Twitter4jPublisher implements TwitterPublisher {

    private final TwitterClientFactory clientFactory;

    @Override
    public TwitterPublishingStatus publish(final Tweet tweet) {
        return Try.of(clientFactory::getClient)
            .mapTry(TwitterV1::tweets)
            .mapTry(tweets -> tweets.updateStatus(tweet.getContent()))
            .onSuccess(status -> log.info("Tweet published successfully {}", status))
            .map(status -> SUCCESS)
            .onFailure(ex -> log.error("Cannot publish a tweet", ex))
            .recover(ex -> ERROR)
            .get();
    }
}
