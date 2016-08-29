package com.jvm_bloggers.frontend.newsletter_issue;

import lombok.Builder;

@Builder
public class BlogPostDto {

    public String url;
    public String title;
    public String authorName;
    public String authorTwitterHandle;
    public BlogTypeDto blogType;

}
