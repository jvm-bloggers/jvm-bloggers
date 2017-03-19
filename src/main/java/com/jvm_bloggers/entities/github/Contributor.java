package com.jvm_bloggers.entities.github;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Data;

import java.io.Serializable;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class Contributor implements Serializable {

    private String login;
    @JsonProperty("html_url")
    private String profilePage;
    @JsonProperty("avatar_url")
    private String avatarUrl;
    private int contributions;
}
