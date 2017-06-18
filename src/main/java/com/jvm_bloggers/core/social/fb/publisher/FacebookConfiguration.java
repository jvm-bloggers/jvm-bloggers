package com.jvm_bloggers.core.social.fb.publisher;

import lombok.Data;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

import javax.validation.constraints.NotNull;

@Data
@Validated
@ConfigurationProperties("facebook.api")
public class FacebookConfiguration {

    @NotNull
    private String pageId;
    @NotNull
    private String appSecret;
    @NotNull
    private String userToken;

}
