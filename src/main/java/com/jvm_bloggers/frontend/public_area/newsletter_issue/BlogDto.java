package com.jvm_bloggers.frontend.public_area.newsletter_issue;

import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog;

import lombok.Builder;

import java.util.List;
import java.util.stream.Collectors;

@Builder
public class BlogDto {

    public String author;
    public String url;
    public String authorTwitterHandle;
    public BlogTypeDto type;

    static BlogDto fromBlog(Blog blog) {
        return BlogDto.builder()
            .author(blog.getAuthor())
            .url(blog.getUrl())
            .authorTwitterHandle(blog.getTwitter())
            .type(BlogTypeDto.fromBlogType(blog.getBlogType()))
            .build();
    }

    static List<BlogDto> fromBlogs(List<Blog> blogs) {
        return blogs.stream()
            .map(BlogDto::fromBlog)
            .collect(Collectors.toList());
    }
}
