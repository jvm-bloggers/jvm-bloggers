package pl.tomaszdziurko.jvm_bloggers.rest.newsletter_issue;

import lombok.Builder;

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;

import java.util.List;
import java.util.stream.Collectors;

@Builder
class BlogDto {

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
