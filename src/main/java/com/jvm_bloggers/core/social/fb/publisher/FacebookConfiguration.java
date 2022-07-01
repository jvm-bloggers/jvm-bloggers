package com.jvm_bloggers.core.social.fb.publisher;

import lombok.Data;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

import lombok.NonNull;

@Data
//@Validated
@ConfigurationProperties("facebook.api")
public class FacebookConfiguration {

//    @NonNull
    private String pageId;
//    @NonNull
    private String appSecret;
//    @NonNull
    private String userToken;

}
