package com.jvm_bloggers.core.social.twitter.publisher;

import com.jvm_bloggers.entities.twitter.Tweet;
import io.vavr.control.Try;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import static com.jvm_bloggers.ApplicationProfiles.PRODUCTION;
import static com.jvm_bloggers.core.social.twitter.publisher.TwitterPublisher.TwitterPublishingStatus.ERROR;
import static com.jvm_bloggers.core.social.twitter.publisher.TwitterPublisher.TwitterPublishingStatus.SUCCESS;
import static lombok.AccessLevel.PACKAGE;
import static org.slf4j.LoggerFactory.getLogger;

@Component
@Profile(PRODUCTION)
@RequiredArgsConstructor(access = PACKAGE)
class Twitter4jPublisher implements TwitterPublisher {

    private static final Logger LOG = getLogger(Twitter4jPublisher.class);

    private final TwitterClientFactory clientFactory;

    @Override
    public TwitterPublishingStatus publish(Tweet tweet) {
        return Try.of(() -> clientFactory.getClient())
            .mapTry(twitter -> twitter.updateStatus(tweet.getContent()))
            .onSuccess(status -> LOG.info("Tweet published successfully {}", status))
            .map(status -> SUCCESS)
            .onFailure(ex -> LOG.error("Cannot publish a tweet", ex))
            .recover(ex -> ERROR)
            .get();
    }

}
