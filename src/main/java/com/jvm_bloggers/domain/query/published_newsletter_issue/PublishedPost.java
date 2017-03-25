package com.jvm_bloggers.domain.query.published_newsletter_issue;

import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class PublishedPost {

    private String url;
    private String title;
    private String authorName;
    private String authorTwitterHandle;

}
