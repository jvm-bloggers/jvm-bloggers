package com.jvm_bloggers.core.social.twitter.publisher;

import com.jvm_bloggers.entities.twitter.Tweet;

import lombok.extern.slf4j.Slf4j;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import static com.jvm_bloggers.ApplicationProfiles.DEV;
import static com.jvm_bloggers.ApplicationProfiles.STAGE;
import static com.jvm_bloggers.ApplicationProfiles.TEST;

@Component
@Profile({DEV, STAGE, TEST})
@Slf4j
class LogTwitterPublisher implements TwitterPublisher {

    @Override
    public TwitterPublishingStatus publish(Tweet tweet) {
        log.debug("Publishing on Twitter: {}", tweet.getContent());
        return TwitterPublishingStatus.SUCCESS;
    }

}
