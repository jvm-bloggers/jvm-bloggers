package com.jvm_bloggers.frontend.newsletter_issue;


import lombok.Builder;

import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;


@Builder
public class NewsletterIssueDto {

    public long number;
    public LocalDate publishedDate;
    public String heading;
    public String varia;
    public List<BlogPostDto> posts;
    public List<BlogDto> newBlogs;

    public List<BlogPostDto> getPersonalPosts() {
        return filterBlogPosts(BlogTypeDto.PERSONAL);
    }

    public List<BlogPostDto> getCompaniesPosts() {
        return filterBlogPosts(BlogTypeDto.COMPANY);
    }

    public List<BlogPostDto> getVideos() {
        return filterBlogPosts(BlogTypeDto.VIDEOS);
    }

    private List<BlogPostDto> filterBlogPosts(BlogTypeDto type) {
        return posts.stream().filter(p -> p.blogType == type).collect(Collectors.toList());
    }

}
