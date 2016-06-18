package pl.tomaszdziurko.jvm_bloggers.rest.newsletter_issue;


import lombok.Builder;

import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue;

import java.time.LocalDate;
import java.util.List;

import static pl.tomaszdziurko.jvm_bloggers.rest.newsletter_issue.BlogDto.fromBlogs;
import static pl.tomaszdziurko.jvm_bloggers.rest.newsletter_issue.BlogPostDto.fromBlogPosts;

@Builder
class NewsletterIssueDto {

    public long number;
    public LocalDate publishedDate;
    public String heading;
    public String varia;
    public List<BlogPostDto> posts;
    public List<BlogDto> newBlogs;

    static NewsletterIssueDto fromIssue(NewsletterIssue issue) {
        return NewsletterIssueDto.builder()
            .number(issue.getIssueNumber())
            .heading(issue.getHeading())
            .varia(issue.getVaria())
            .publishedDate(issue.getPublishedDate())
            .newBlogs(fromBlogs(issue.getNewBlogs()))
            .posts(fromBlogPosts(issue.getBlogPosts()))
            .build();
    }

}
