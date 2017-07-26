package com.jvm_bloggers.core.social.twitter.publisher;

import lombok.Data;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

import javax.validation.constraints.NotNull;

@Data
@Validated
@ConfigurationProperties("twitter.api")
public class TwitterConfiguration {

    @NotNull
    private String consumerKey;
    @NotNull
    private String consumerSecret;
    @NotNull
    private String accessToken;
    @NotNull
    private String accessTokenSecret;
    @NotNull
    private int retryCount;
    @NotNull
    private int retryIntervalSecs;

}
