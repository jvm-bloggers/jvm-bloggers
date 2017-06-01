package com.jvm_bloggers.core.social.fb.publisher;

import com.jvm_bloggers.entities.fb.FacebookPost;
import com.restfb.FacebookClient;
import com.restfb.Parameter;
import com.restfb.types.FacebookType;
import com.restfb.types.Post;

import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.io.IOException;

import static com.jvm_bloggers.ApplicationProfiles.PRODUCTION;

@Slf4j
@Component
@Profile({PRODUCTION})
class RestFbPublisher {

    private final FacebookClient facebook;

    @Autowired
    RestFbPublisher(RestFbFacebookClientFactory clientFactory) throws IOException {
        this.facebook = clientFactory.createFacebookClient();
    }

    public String publishPost(FacebookPost post) {
        log.info("Sending a new message to Facebook: {}", post);
        final FacebookType publishResponse =
            facebook.publish("me/feed", Post.class,
                Parameter.with("message", post.getMessage()),
                Parameter.with("link", post.getIssueLink()));
        log.info("Message published with id {}", publishResponse.getId());
        return publishResponse.getId();
    }

}
