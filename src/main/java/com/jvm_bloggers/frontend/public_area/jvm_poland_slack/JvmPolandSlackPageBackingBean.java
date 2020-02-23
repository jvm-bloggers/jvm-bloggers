package com.jvm_bloggers.frontend.public_area.jvm_poland_slack;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class JvmPolandSlackPageBackingBean {

    private final String invitationLink;

    @Autowired
    public JvmPolandSlackPageBackingBean(@Value("${slack.invitation-link}") String invitationLink) {
        this.invitationLink = invitationLink;
    }

    public String getInvitationLink() {
        return invitationLink;
    }
}
