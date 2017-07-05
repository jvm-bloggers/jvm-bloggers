package com.jvm_bloggers.core.social.twitter.publisher;

import com.jvm_bloggers.entities.twitter.Tweet;

interface TwitterPublisher {

    TwitterPublishingStatus publish(Tweet tweet);

    enum TwitterPublishingStatus {
        SUCCESS,
        ERROR;

        public boolean isOk() {
            return this.equals(SUCCESS);
        }
    }

}
