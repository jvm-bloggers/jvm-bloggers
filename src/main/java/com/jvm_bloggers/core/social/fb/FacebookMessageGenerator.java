package com.jvm_bloggers.core.social.fb;

import org.springframework.stereotype.Component;

import static java.lang.String.format;

@Component
class FacebookMessageGenerator {

    public String generateFacebookMessage(String link,
                                          FacebookMessageTemplate facebookMessageTemplate) {
        return format(facebookMessageTemplate.getTemplate(), link);
    }

}
