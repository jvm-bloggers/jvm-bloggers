package com.jvm_bloggers.frontend.admin_area;

import com.jvm_bloggers.domain.command.CommandPublisher;
import com.jvm_bloggers.domain.command.new_facebook_post.CreateNewFacebookPost;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class AdminSocialChannelsPageBackingBean {

    private final CommandPublisher commandPublisher;

    public void createNewFacebookPost(String message, String link) {
        commandPublisher.publish(
            new CreateNewFacebookPost(message, link)
        );
    }

}
