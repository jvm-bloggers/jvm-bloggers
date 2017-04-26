package com.jvm_bloggers.domain.query.published_newsletter_issue;

import com.jvm_bloggers.domain.query.NewsletterIssueNumber;
import javaslang.collection.List;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDate;

@Builder
@Getter
public class PublishedNewsletterIssue {

    private NewsletterIssueNumber number;
    private LocalDate publishedDate;
    private String headingSection;
    private String variaSection;
    private List<PublishedPost> personalPosts;
    private List<PublishedPost> companyPosts;
    private List<PublishedPost> videos;
    private List<NewlyAddedBlog> newBlogs;

}
