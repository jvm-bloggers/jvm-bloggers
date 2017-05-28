package com.jvm_bloggers.core.social.fb.publisher;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.restfb.DefaultFacebookClient;
import com.restfb.Version;
import com.restfb.WebRequestor;

import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

import java.io.IOException;

import javaslang.control.Try;

import static com.jvm_bloggers.ApplicationProfiles.PRODUCTION;
import static lombok.AccessLevel.PRIVATE;

@Component
@Slf4j
@Profile({PRODUCTION})
class PageAccessTokenProvider {

    private static final String
        PAGE_TOKEN_URL =
        "https://graph.facebook.com/%s?fields=access_token&access_token=%s&appsecret_proof=%s";

    private final DefaultFacebookClient facebookClient;
    private final String pageId;
    private final String appSecret;
    private final String userAccessToken;
    private final ObjectMapper objectMapper;

    @Autowired
    PageAccessTokenProvider(Environment env) {
        facebookClient = new DefaultFacebookClient(Version.LATEST);
        pageId = env.getProperty("fb.page.id");
        appSecret = env.getProperty("fb.app.secret");
        userAccessToken = env.getProperty("fb.user.token");
        objectMapper = new ObjectMapper();
    }

    String getToken() {
        log.info("Fetching application secret proof...");
        String appSecretProof = facebookClient.obtainAppSecretProof(userAccessToken, appSecret);
        log.info("Requesting page access token...");
        return Try.of(() -> queryForPageToken(appSecretProof))
            .mapTry(response -> extractToken(response))
            .getOrElseThrow(
                ex -> new PageAccessTokenException("Cannot fetch page access token from FB", ex));
    }

    private WebRequestor.Response queryForPageToken(String appSecretProof) throws IOException {
        return facebookClient.getWebRequestor().executeGet(
            String.format(PAGE_TOKEN_URL, pageId, userAccessToken, appSecretProof)
        );
    }

    private String extractToken(WebRequestor.Response response) throws IOException {
        return objectMapper
            .readValue(response.getBody(), PageAccessToken.class)
            .getToken();
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    @NoArgsConstructor(access = PRIVATE)
    class PageAccessToken {

        @JsonProperty("access_token")
        private String token;

        String getToken() {
            return token;
        }
    }

    class PageAccessTokenException extends RuntimeException {

        PageAccessTokenException(String message, Throwable cause) {
            super(message, cause);
        }
    }

}
