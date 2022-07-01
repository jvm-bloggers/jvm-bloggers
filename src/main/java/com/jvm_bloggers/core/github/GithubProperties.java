package com.jvm_bloggers.core.github;

import lombok.Data;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.springframework.validation.annotation.Validated;

import lombok.NonNull;

@Data
//@Validated
@ConfigurationProperties("github.api")
public class GithubProperties {

    private String token;
//    @NonNull
    private String apiUrl;
//    @NonNull
    private String org;
//    @NonNull
    private String repo;
//    @NonNull
    private String pageSize;

}
