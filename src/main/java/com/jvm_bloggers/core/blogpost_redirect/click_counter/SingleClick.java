package com.jvm_bloggers.core.blogpost_redirect.click_counter;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Value;

@RequiredArgsConstructor
@Getter
public class SingleClick {

    @NonNull
    private final BlogPost blogPost;

    @NonNull
    private final Referer referer;

    @NonNull
    private final UserAgent userAgent;

    @NonNull
    private final IpAddress ip;

    @Value
    public static class Referer {
        private final String value;
    }

    @Value
    public static class UserAgent {
        private final String value;
    }

    @Value
    public static class IpAddress {
        private final String value;
    }

}
