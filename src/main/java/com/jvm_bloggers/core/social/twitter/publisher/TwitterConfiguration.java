package com.jvm_bloggers.core.social.twitter.publisher;

import lombok.Data;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

import lombok.NonNull;

@Data
//@Validated
@ConfigurationProperties("twitter.api")
public class TwitterConfiguration {

//    @NonNull
    private String consumerKey;
//    @NonNull
    private String consumerSecret;
//    @NonNull
    private String accessToken;
//    @NonNull
    private String accessTokenSecret;

    private int retryCount;

    private int retryIntervalSecs;

}
