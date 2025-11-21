package com.jvm_bloggers.core.social.twitter.publisher;

import twitter4j.Twitter;
import twitter4j.v1.TwitterV1;

class TwitterClientFactory {

    private final TwitterConfiguration configuration;

    TwitterClientFactory(final TwitterConfiguration configuration) {
        this.configuration = configuration;
    }

    public TwitterV1 getClient() {
        return Twitter.newBuilder()
            .oAuthConsumer(configuration.getConsumerKey(), configuration.getConsumerSecret())
            .oAuthAccessToken(configuration.getAccessToken(), configuration.getAccessTokenSecret())
            .httpRetryCount(configuration.getRetryCount())
            .httpRetryIntervalSeconds(configuration.getRetryIntervalSecs())
            .build()
            .v1();
    }
}
