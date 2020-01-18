package com.jvm_bloggers.core.social.fb.publisher;

import com.jvm_bloggers.entities.fb.FacebookPost;

import lombok.extern.slf4j.Slf4j;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import static com.jvm_bloggers.ApplicationProfiles.DEV;
import static com.jvm_bloggers.ApplicationProfiles.STAGE;
import static com.jvm_bloggers.ApplicationProfiles.TEST;

@Component
@Profile({DEV, STAGE, TEST})
@Slf4j
class LogFacebookPublisher implements FacebookPublisher {

    @Override
    public FacebookPublishingStatus publishPost(FacebookPost post) {
        log.debug("Publishing on Facebook page: link {}, message {}", post.getLink(),
            post.getMessage());
        return FacebookPublishingStatus.SUCCESS;
    }

}
