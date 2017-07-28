package com.jvm_bloggers.domain.query.published_newsletter_issue;

import com.jvm_bloggers.entities.blog.Blog;

import io.vavr.collection.Seq;

import lombok.Value;

@Value
public class NewlyAddedBlog {

    private final String author;
    private final String url;
    private final String authorTwitterHandle;

    static NewlyAddedBlog fromBlog(Blog blog) {
        return new NewlyAddedBlog(blog.getAuthor(), blog.getUrl(), blog.getTwitter());
    }

    static Seq<NewlyAddedBlog> fromBlogs(Seq<Blog> blogs) {
        return blogs.map(NewlyAddedBlog::fromBlog);
    }

}
