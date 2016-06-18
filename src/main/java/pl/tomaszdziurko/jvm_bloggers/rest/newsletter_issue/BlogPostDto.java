package pl.tomaszdziurko.jvm_bloggers.rest.newsletter_issue;

import lombok.Builder;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;

import java.util.List;
import java.util.stream.Collectors;

@Builder
class BlogPostDto {

    public String url;
    public String title;
    public String authorName;
    public String authorTwitterHandle;
    public BlogTypeDto blogType;

    static BlogPostDto fromBlogPost(BlogPost blogPost) {
        return BlogPostDto.builder()
            .url(blogPost.getUrl())
            .title(blogPost.getTitle())
            .authorName(blogPost.getBlog().getAuthor())
            .authorTwitterHandle(blogPost.getBlog().getTwitter())
            .blogType(BlogTypeDto.fromBlogType(blogPost.getBlog().getBlogType()))
            .build();
    }

    static List<BlogPostDto> fromBlogPosts(List<BlogPost> blogPosts) {
        return blogPosts.stream()
            .map(BlogPostDto::fromBlogPost)
            .collect(Collectors.toList());
    }
}
