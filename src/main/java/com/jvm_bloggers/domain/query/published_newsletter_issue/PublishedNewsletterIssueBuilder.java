package com.jvm_bloggers.domain.query.published_newsletter_issue;

import com.google.common.annotations.VisibleForTesting;
import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator;
import com.jvm_bloggers.domain.query.NewsletterIssueNumber;
import com.jvm_bloggers.entities.blog.BlogType;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import io.vavr.collection.Seq;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;

import static com.jvm_bloggers.entities.blog.BlogType.COMPANY;
import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL;
import static com.jvm_bloggers.entities.blog.BlogType.PODCAST;
import static com.jvm_bloggers.entities.blog.BlogType.PRESENTATION;
import static io.vavr.collection.List.ofAll;

@Component
@AllArgsConstructor
public class PublishedNewsletterIssueBuilder {

    private final LinkGenerator linkGenerator;

    public PublishedNewsletterIssue build(NewsletterIssue issue) {
        return PublishedNewsletterIssue.builder()
                .number(NewsletterIssueNumber.of(issue.getIssueNumber()))
                .headingSection(issue.getHeading())
                .variaSection(issue.getVaria())
                .publishedDate(issue.getPublishedDate())
                .newBlogs(NewlyAddedBlog.fromBlogs(ofAll(issue.getNewBlogs())))
                .personalPosts(filterBlogPosts(issue.getBlogPosts(), PERSONAL))
                .companyPosts(filterBlogPosts(issue.getBlogPosts(), COMPANY))
                .presentations(filterBlogPosts(issue.getBlogPosts(), PRESENTATION))
                .podcasts(filterBlogPosts(issue.getBlogPosts(), PODCAST))
                .build();
    }

    private Seq<PublishedPost> filterBlogPosts(java.util.List<BlogPost> posts, BlogType type) {
        return ofAll(posts)
                .filter(p -> p.getBlog().getBlogType() == type)
                .map(this::fromBlogPost);
    }

    @VisibleForTesting
    PublishedPost fromBlogPost(BlogPost blogPost) {
        return PublishedPost.builder()
                .url(linkGenerator.generateRedirectLinkFor(blogPost.getUid()))
                .title(blogPost.getTitle())
                .authorName(blogPost.getBlog().getAuthor())
                .authorTwitterHandle(blogPost.getBlog().getTwitter())
                .build();
    }

}
