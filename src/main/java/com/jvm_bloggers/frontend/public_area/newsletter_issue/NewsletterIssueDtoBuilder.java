package com.jvm_bloggers.frontend.public_area.newsletter_issue;

import com.google.common.annotations.VisibleForTesting;
import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator;
import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPost;
import com.jvm_bloggers.core.newsletter_issues.domain.NewsletterIssue;
import com.jvm_bloggers.core.newsletter_issues.domain.NewsletterIssueBaseData;

import lombok.AllArgsConstructor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

import static com.jvm_bloggers.frontend.public_area.newsletter_issue.BlogDto.fromBlogs;

@Component
@AllArgsConstructor(onConstructor = @__(@Autowired))
public class NewsletterIssueDtoBuilder {

    private final LinkGenerator linkGenerator;

    public NewsletterIssueDto build(NewsletterIssue issue) {
        return NewsletterIssueDto.builder()
            .number(issue.getIssueNumber())
            .heading(issue.getHeading())
            .varia(issue.getVaria())
            .publishedDate(issue.getPublishedDate())
            .newBlogs(fromBlogs(issue.getNewBlogs()))
            .posts(fromBlogPosts(issue.getBlogPosts()))
            .build();
    }

    public NewsletterIssueDto build(NewsletterIssueBaseData issue) {
        return NewsletterIssueDto.builder()
            .number(issue.getIssueNumber())
            .publishedDate(issue.getPublishedDate())
            .build();
    }

    private List<BlogPostDto> fromBlogPosts(List<BlogPost> blogPosts) {
        return blogPosts.stream()
            .map(this::fromBlogPost)
            .collect(Collectors.toList());
    }

    @VisibleForTesting
    BlogPostDto fromBlogPost(BlogPost blogPost) {
        return BlogPostDto.builder()
            .url(linkGenerator.generateRedirectLinkFor(blogPost.getUid()))
            .title(blogPost.getTitle())
            .authorName(blogPost.getBlog().getAuthor())
            .authorTwitterHandle(blogPost.getBlog().getTwitter())
            .blogType(BlogTypeDto.fromBlogType(blogPost.getBlog().getBlogType()))
            .build();
    }

}
