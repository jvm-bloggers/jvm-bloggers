package com.jvm_bloggers.core.blogpost_redirect.click_counter;

import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPost;

import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public class SingleClick {

    @NonNull
    private final BlogPost blogPost;

}
