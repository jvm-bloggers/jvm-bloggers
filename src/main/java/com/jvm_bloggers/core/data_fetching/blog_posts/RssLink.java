package com.jvm_bloggers.core.data_fetching.blog_posts;

import com.google.common.base.Preconditions;
import com.jvm_bloggers.entities.blog.Blog;
import lombok.NonNull;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

public class RssLink {

    private final Blog blog;

    public RssLink(@NonNull Blog blog) {
        Preconditions.checkArgument(isNotBlank(blog.getRss()),
                "Rss link can not be NULL nor empty.");
        this.blog = blog;
    }

    public String getUrl() {
        return blog.getRss();
    }

    public Blog getOwner() {
        return blog;
    }
}
