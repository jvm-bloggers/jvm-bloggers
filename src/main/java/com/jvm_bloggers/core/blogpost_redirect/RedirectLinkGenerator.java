package com.jvm_bloggers.core.blogpost_redirect;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import static com.jvm_bloggers.core.blogpost_redirect.RedirectController.REDIRECT_URL_PATH;

@Component
public class RedirectLinkGenerator {

    private final String applicationBaseUrl;

    @Autowired
    public RedirectLinkGenerator(@Value("${application.baseUrl}") String applicationBaseUrl) {
        this.applicationBaseUrl = applicationBaseUrl;
    }

    public String generateLinkFor(String blogPostUid) {
        return applicationBaseUrl + REDIRECT_URL_PATH + "/" + blogPostUid;
    }
}
