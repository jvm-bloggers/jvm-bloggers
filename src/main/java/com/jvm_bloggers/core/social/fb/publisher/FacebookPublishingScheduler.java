package com.jvm_bloggers.core.social.fb.publisher;

import com.jvm_bloggers.core.social.fb.publisher.FacebookPublisher.FacebookPublishingStatus;
import com.jvm_bloggers.entities.fb.FacebookPost;
import com.jvm_bloggers.entities.fb.FacebookPostRepository;
import com.jvm_bloggers.utils.NowProvider;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import io.vavr.control.Option;

import static lombok.AccessLevel.PACKAGE;

@Component
@Slf4j
@RequiredArgsConstructor(access = PACKAGE)
class FacebookPublishingScheduler {

    private final FacebookPostRepository facebookPostRepository;
    private final FacebookPublisher facebookPublisher;
    private final NowProvider nowProvider;

    @Scheduled(fixedDelayString = "${scheduler.publish-fb}")
    public void publishOnePost() {
        Option<FacebookPost> notSentPost = facebookPostRepository.findFirstBySentDateNull();

        notSentPost.forEach(post -> {
            final FacebookPublishingStatus status = facebookPublisher.publishPost(post);
            log.info("Post on Facebook published, status: {}", status);
            setSentDateForSuccessfulAction(post, status);
        });
    }

    private void setSentDateForSuccessfulAction(
        FacebookPost facebookPost,
        FacebookPublishingStatus status) {
        if (status.isOk()) {
            facebookPost.setSentDate(nowProvider.now());
            facebookPostRepository.save(facebookPost);
        }
    }

}
