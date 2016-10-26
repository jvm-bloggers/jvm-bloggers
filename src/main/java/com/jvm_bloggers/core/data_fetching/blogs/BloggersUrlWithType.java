package com.jvm_bloggers.core.data_fetching.blogs;

import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogType;

import lombok.Data;
import lombok.NonNull;

import java.net.URL;
import java.util.Optional;

@Data
public class BloggersUrlWithType {

    @NonNull
    private final Optional<URL> blogsDataUrl;
    @NonNull
    private final BlogType blogType;
}
