package com.jvm_bloggers.core.social.fb.publisher;

import com.jvm_bloggers.entities.fb.FacebookPost;

interface FacebookPublisher {

    FacebookPublishingStatus publishPost(FacebookPost post);

    enum FacebookPublishingStatus {
        SUCCESS,
        ERROR;

        public boolean isOk() {
            return this.equals(SUCCESS);
        }
    }

}
