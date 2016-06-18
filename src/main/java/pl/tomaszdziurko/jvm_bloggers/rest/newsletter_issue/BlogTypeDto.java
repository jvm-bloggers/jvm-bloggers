package pl.tomaszdziurko.jvm_bloggers.rest.newsletter_issue;

import com.google.common.collect.ImmutableMap;

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType;

import java.util.Map;

enum BlogTypeDto {

    PERSONAL, COMPANY, VIDEOS;

    private static Map<BlogType, BlogTypeDto> mappings =
        ImmutableMap.<BlogType, BlogTypeDto>builder()
            .put(BlogType.PERSONAL, PERSONAL)
            .put(BlogType.COMPANY, COMPANY)
            .put(BlogType.VIDEOS, VIDEOS)
            .build();

    static BlogTypeDto fromBlogType(BlogType blogType) {
        return mappings.get(blogType);
    }

}
