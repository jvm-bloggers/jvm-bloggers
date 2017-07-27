package com.jvm_bloggers.domain.query.blog_post_for_listing;

import com.jvm_bloggers.entities.blog_post.BlogPost;

import lombok.Value;

import java.io.Serializable;
import java.time.LocalDateTime;

@Value
public class BlogPostForListing implements Serializable {

    private String uid;
    private String title;
    private LocalDateTime publishedDate;

    public static BlogPostForListing fromBlogPost(BlogPost blogPost) {
        return new BlogPostForListing(
            blogPost.getUid(),
            blogPost.getTitle(),
            blogPost.getPublishedDate());
    }
}
