package com.jvm_bloggers.core.newsletter_issues;

import com.jvm_bloggers.core.mailing.IssueNumberRetriever;
import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.entities.metadata.MetadataKeys;
import com.jvm_bloggers.entities.metadata.MetadataRepository;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import com.jvm_bloggers.utils.NowProvider;

import lombok.AllArgsConstructor;

import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.List;

@Component
@AllArgsConstructor
public class NewsletterIssueFactory {

    private final IssueNumberRetriever issueNumberRetriever;
    private final NowProvider nowProvider;
    private final BlogRepository blogRepository;
    private final BlogPostRepository blogPostRepository;
    private final MetadataRepository metadataRepository;

    public NewsletterIssue create(int daysInThePastToIncludeInNewIssue, long issueNumber) {

        LocalDateTime startDate = calculateStartDate(daysInThePastToIncludeInNewIssue);
        List<Blog> newBlogs = blogRepository.findByDateAddedAfter(startDate);
        List<BlogPost> newApprovedPosts = blogPostRepository
            .findByApprovedDateAfterAndApprovedTrueOrderByApprovedDateAsc(startDate);

        return NewsletterIssue.builder()
            .issueNumber(issueNumber)
            .publishedDate(nowProvider.today())
            .newBlogs(newBlogs)
            .blogPosts(newApprovedPosts)
            .heading(metadataRepository.findByName(MetadataKeys.HEADING_TEMPLATE).getValue())
            .varia(metadataRepository.findByName(MetadataKeys.VARIA_TEMPLATE).getValue())
            .build();
    }

    public NewsletterIssue create(int daysInThePastToIncludeInNewIssue) {
        long nextIssueNumber = issueNumberRetriever.getNextIssueNumber();
        return create(daysInThePastToIncludeInNewIssue, nextIssueNumber);
    }

    private LocalDateTime calculateStartDate(int daysInThePastToIncludeInNewIssue) {
        return nowProvider.now()
            .minusDays(daysInThePastToIncludeInNewIssue)
            .withHour(10)
            .withMinute(0)
            .withSecond(0)
            .withNano(0);
    }
}
