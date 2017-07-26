package com.jvm_bloggers.domain.command.new_facebook_post;

import com.jvm_bloggers.domain.command.Command;
import lombok.RequiredArgsConstructor;
import lombok.Value;

@Value
@RequiredArgsConstructor
public class CreateNewFacebookPost implements Command {

    private String facebookMessage;
    private String issueLink;

}
