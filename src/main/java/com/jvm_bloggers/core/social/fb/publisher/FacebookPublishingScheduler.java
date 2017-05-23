package com.jvm_bloggers.core.social.fb.publisher;

import com.jvm_bloggers.entities.fb.FacebookPost;
import com.jvm_bloggers.entities.fb.FacebookPostRepository;
import com.jvm_bloggers.utils.NowProvider;
import javaslang.control.Option;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@Slf4j
class FacebookPublishingScheduler {

    private final FacebookPostRepository facebookPostRepository;
    private final FacebookPublisher facebookPublisher;
    private final NowProvider nowProvider;

    @Autowired
    FacebookPublishingScheduler(
        FacebookPostRepository facebookPostRepository,
        FacebookPublisher facebookPublisher,
        NowProvider nowProvider
    ) {
        this.facebookPostRepository = facebookPostRepository;
        this.facebookPublisher = facebookPublisher;
        this.nowProvider = nowProvider;
    }

    @Scheduled(fixedDelayString = "${scheduler.publish-fb}")
    void publishOnePost() {
        Option<FacebookPost> notSentEmail = facebookPostRepository.findFirstBySentDateNull();

        notSentEmail.forEach(post -> {
            FacebookPublisher.FacebookPublishingStatus status = facebookPublisher.publishPost(
                post.getIssueLink(),
                post.getMessage()
            );
            log.info("Post on Facebook published, status: {}", status);
            setSentDateForSuccessfulAction(post, status);
        });
    }

    private void setSentDateForSuccessfulAction(
        FacebookPost facebookPost,
        FacebookPublisher.FacebookPublishingStatus status) {
        if (status.isOk()) {
            facebookPost.setSentDate(nowProvider.now());
            facebookPostRepository.save(facebookPost);
        }
    }

}
