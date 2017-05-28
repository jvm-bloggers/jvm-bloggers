package com.jvm_bloggers.core.social.fb;

import org.springframework.stereotype.Component;

@Component
class FacebookMessageGenerator {

    private static final String MESSAGE_TEMPLATE =
        "Nowe wydanie JVM Bloggers czeka już na Was: %s #java #newsletter #jvmbloggers #blogs";

    String generateFacebookMessage(String issueLink) {
        return String.format(MESSAGE_TEMPLATE, issueLink);
    }

}
