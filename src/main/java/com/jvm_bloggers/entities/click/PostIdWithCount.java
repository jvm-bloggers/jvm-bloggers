package com.jvm_bloggers.entities.click;

import lombok.Value;

@Value
public class PostIdWithCount {

    private final Long blogPostId;
    private final Long count;

}
