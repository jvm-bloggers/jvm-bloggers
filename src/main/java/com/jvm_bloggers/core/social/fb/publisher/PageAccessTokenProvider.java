package com.jvm_bloggers.core.social.fb.publisher;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.restfb.DefaultFacebookClient;
import com.restfb.Version;
import com.restfb.WebRequestor;
import io.vavr.control.Try;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.io.IOException;

import static com.jvm_bloggers.ApplicationProfiles.PRODUCTION;
import static java.lang.String.format;
import static lombok.AccessLevel.PROTECTED;

@Component
@Slf4j
@Profile(PRODUCTION)
class PageAccessTokenProvider {

    private static final String
        PAGE_TOKEN_URL =
        "https://graph.facebook.com/%s?fields=access_token&access_token=%s&appsecret_proof=%s";

    private final DefaultFacebookClient facebookClient = new DefaultFacebookClient(Version.LATEST);
    private final FacebookConfiguration facebookConfiguration;
    private final ObjectMapper objectMapper;

    @Autowired
    PageAccessTokenProvider(FacebookConfiguration facebookConfiguration, ObjectMapper mapper) {
        this.facebookConfiguration = facebookConfiguration;
        this.objectMapper = mapper;
    }

    public String getToken() {
        log.info("Fetching application secret proof...");
        final String appSecretProof = facebookClient.obtainAppSecretProof(
            facebookConfiguration.getUserToken(),
            facebookConfiguration.getAppSecret()
        );
        log.info("Requesting page access token...");
        return Try
            .of(() -> queryForPageToken(appSecretProof))
            .mapTry(this::extractToken)
            .getOrElseThrow(
                ex -> new PageAccessTokenException("Cannot fetch page access token from FB", ex)
            );
    }

    private WebRequestor.Response queryForPageToken(String appSecretProof) throws IOException {
        return facebookClient.getWebRequestor().executeGet(
            format(
                PAGE_TOKEN_URL, facebookConfiguration.getPageId(),
                facebookConfiguration.getUserToken(), appSecretProof
            )
        );
    }

    private String extractToken(WebRequestor.Response response) throws IOException {
        return objectMapper
            .readValue(response.getBody(), PageAccessToken.class)
            .getToken();
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    @NoArgsConstructor(access = PROTECTED)
    private static class PageAccessToken {

        @JsonProperty("access_token")
        private String token;

        private String getToken() {
            return token;
        }
    }

    class PageAccessTokenException extends RuntimeException {

        PageAccessTokenException(String message, Throwable cause) {
            super(message, cause);
        }
    }

}
