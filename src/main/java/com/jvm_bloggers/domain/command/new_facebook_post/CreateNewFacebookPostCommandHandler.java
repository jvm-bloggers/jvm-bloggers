package com.jvm_bloggers.domain.command.new_facebook_post;

import com.jvm_bloggers.domain.command.CommandHandler;
import com.jvm_bloggers.entities.fb.FacebookPost;
import com.jvm_bloggers.entities.fb.FacebookPostRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class CreateNewFacebookPostCommandHandler implements CommandHandler<CreateNewFacebookPost> {


    private final FacebookPostRepository facebookPostRepository;

    @Override
    public void handle(CreateNewFacebookPost command) {



    }
}