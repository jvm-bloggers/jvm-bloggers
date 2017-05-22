package com.jvm_bloggers.core.social.fb.publisher;

interface FacebookPublisher {

    FacebookPublishingStatus publishPost(String issueLink, String postMessage);

    enum FacebookPublishingStatus {
        SUCCESS,
        ERROR;

        public boolean isOk() {
            return this.equals(SUCCESS);
        }
    }

}
