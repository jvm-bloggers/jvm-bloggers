package pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue;

import com.google.common.annotations.VisibleForTesting;

import lombok.AllArgsConstructor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.click_counter.RedirectLinkGenerator;
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue;

import java.util.List;
import java.util.stream.Collectors;

import static pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue.BlogDto.fromBlogs;

@Component
@AllArgsConstructor(onConstructor = @__(@Autowired))
public class NewsletterIssueDtoBuilder {

    private final RedirectLinkGenerator redirectLinkGenerator;

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

    private List<BlogPostDto> fromBlogPosts(List<BlogPost> blogPosts) {
        return blogPosts.stream()
            .map(this::fromBlogPost)
            .collect(Collectors.toList());
    }

    @VisibleForTesting
    BlogPostDto fromBlogPost(BlogPost blogPost) {
        return BlogPostDto.builder()
            .url(redirectLinkGenerator.generateLinkFor(blogPost.getUid()))
            .title(blogPost.getTitle())
            .authorName(blogPost.getBlog().getAuthor())
            .authorTwitterHandle(blogPost.getBlog().getTwitter())
            .blogType(BlogTypeDto.fromBlogType(blogPost.getBlog().getBlogType()))
            .build();
    }

}
