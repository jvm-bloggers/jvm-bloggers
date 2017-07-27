package com.jvm_bloggers.core.social.twitter.publisher;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import twitter4j.Twitter;
import twitter4j.TwitterFactory;
import twitter4j.conf.ConfigurationBuilder;

import static com.jvm_bloggers.ApplicationProfiles.PRODUCTION;

@Component
@Profile(PRODUCTION)
class TwitterClientFactory {

    private final TwitterFactory factory;

    TwitterClientFactory(TwitterConfiguration configuration) {
        final ConfigurationBuilder builder = new ConfigurationBuilder()
            .setOAuthConsumerKey(configuration.getConsumerKey())
            .setOAuthConsumerSecret(configuration.getConsumerSecret())
            .setOAuthAccessToken(configuration.getAccessToken())
            .setOAuthAccessTokenSecret(configuration.getAccessTokenSecret())
            .setHttpRetryCount(configuration.getRetryCount())
            .setHttpRetryIntervalSeconds(configuration.getRetryIntervalSecs());
        this.factory = new TwitterFactory(builder.build());
    }

    public Twitter getClient() {
        return this.factory.getInstance();
    }
}
