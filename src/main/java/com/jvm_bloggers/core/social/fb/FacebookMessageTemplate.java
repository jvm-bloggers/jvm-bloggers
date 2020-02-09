package com.jvm_bloggers.core.social.fb;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum FacebookMessageTemplate {

    NEW_ISSUE_AVAILABLE("Nowe wydanie JVM Bloggers czeka już na Was: %s #java #jvm #blogs"),
    TOP_POSTS_SUMMARY("Podsumowanie najlepszych postów JVM Bloggers czeka już na Was: "
                       + "%s #java #jvm #blogs");

    private final String template;
}
