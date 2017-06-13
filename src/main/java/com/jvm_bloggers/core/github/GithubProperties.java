package com.jvm_bloggers.core.github;

import lombok.Data;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

import javax.validation.constraints.NotNull;

@Data
@Validated
@ConfigurationProperties("github.api")
public class GithubProperties {

    private String token;
    @NotNull
    private String apiUrl;
    @NotNull
    private String org;
    @NotNull
    private String repo;
    @NotNull
    private String pageSize;

}
