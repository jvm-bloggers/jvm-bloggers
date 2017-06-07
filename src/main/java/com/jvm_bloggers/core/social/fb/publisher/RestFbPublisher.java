package com.jvm_bloggers.core.social.fb.publisher;

import com.jvm_bloggers.entities.fb.FacebookPost;
import com.restfb.FacebookClient;
import com.restfb.Parameter;
import com.restfb.types.Post;

import javaslang.control.Try;

import lombok.extern.slf4j.Slf4j;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.io.IOException;

import static com.jvm_bloggers.ApplicationProfiles.PRODUCTION;
import static com.jvm_bloggers.core.social.fb.publisher.FacebookPublisher.FacebookPublishingStatus.ERROR;
import static com.jvm_bloggers.core.social.fb.publisher.FacebookPublisher.FacebookPublishingStatus.SUCCESS;
import static javaslang.API.$;
import static javaslang.API.Case;
import static javaslang.API.Match;
import static javaslang.Patterns.Failure;
import static javaslang.Patterns.Success;

@Slf4j
@Component
@Profile(PRODUCTION)
class RestFbPublisher implements FacebookPublisher {

    private final FacebookClient facebook;

    RestFbPublisher(RestFbFacebookClientFactory clientFactory) throws IOException {
        this.facebook = clientFactory.createFacebookClient();
    }

    public FacebookPublishingStatus publishPost(FacebookPost post) {
        log.info("Sending a new message to Facebook: {}", post);
        return Try.of(() -> publish(post))
            .onSuccess(publishResponse -> log
                .info("Message published with id {}", publishResponse.getId()))
            .onFailure(
                ex -> log.error("Cannot publish message on Facebook page", ex))
            .transform(result -> Match(result).of(
                Case(Success($()), SUCCESS),
                Case(Failure($()), ERROR)
            ));
    }

    private Post publish(FacebookPost post) {
        return facebook.publish("me/feed", Post.class,
            Parameter.with("message", post.getMessage()),
            Parameter.with("link", post.getIssueLink()));
    }

}
