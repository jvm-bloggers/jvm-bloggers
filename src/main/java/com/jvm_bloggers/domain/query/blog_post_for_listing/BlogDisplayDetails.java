package com.jvm_bloggers.domain.query.blog_post_for_listing;

import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog.BlogType;

import lombok.Value;

import java.io.Serializable;

@Value
public class BlogDisplayDetails implements Serializable {

    private String author;
    private BlogType type;
    private String url;

    public static BlogDisplayDetails fromBlog(Blog blog) {
        return new BlogDisplayDetails(
            blog.getAuthor(),
            blog.getBlogType(),
            blog.getUrl());
    }
}
