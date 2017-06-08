package com.jvm_bloggers.domain.query.published_newsletter_issue;

import com.jvm_bloggers.domain.query.NewsletterIssueNumber;

import javaslang.collection.Seq;

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
    private Seq<PublishedPost> personalPosts;
    private Seq<PublishedPost> companyPosts;
    private Seq<PublishedPost> videos;
    private Seq<NewlyAddedBlog> newBlogs;

}
