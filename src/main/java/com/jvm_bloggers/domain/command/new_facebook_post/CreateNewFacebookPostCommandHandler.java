package com.jvm_bloggers.domain.command.new_facebook_post;

import com.jvm_bloggers.domain.command.CommandHandler;
import com.jvm_bloggers.entities.fb.FacebookPost;
import com.jvm_bloggers.entities.fb.FacebookPostRepository;
import com.jvm_bloggers.utils.NowProvider;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
@RequiredArgsConstructor
class CreateNewFacebookPostCommandHandler implements CommandHandler<CreateNewFacebookPost> {

    private final FacebookPostRepository facebookPostRepository;
    private final NowProvider nowProvider;

    @Override
    @EventListener
    @Transactional
    public void handle(CreateNewFacebookPost command) {

        FacebookPost facebookPost = new FacebookPost(
            command.getIssueLink(),
            command.getFacebookMessage(),
            nowProvider.now()
        );

        facebookPostRepository.save(facebookPost);
    }
}
