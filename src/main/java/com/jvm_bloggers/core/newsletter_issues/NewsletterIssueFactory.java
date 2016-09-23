package com.jvm_bloggers.core.newsletter_issues;

import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPost;
import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPostRepository;
import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog;
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogRepository;
import com.jvm_bloggers.core.mailing.IssueNumberRetriever;
import com.jvm_bloggers.core.metadata.MetadataKeys;
import com.jvm_bloggers.core.metadata.MetadataRepository;
import com.jvm_bloggers.core.newsletter_issues.domain.NewsletterIssue;
import com.jvm_bloggers.utils.NowProvider;

import lombok.AllArgsConstructor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.List;

@Component
@AllArgsConstructor(onConstructor = @__(@Autowired))
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
            .findByPublishedDateAfterAndApprovedTrueOrderByPublishedDateAsc(startDate);

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
            .withHour(11)
            .withMinute(0)
            .withSecond(0)
            .withNano(0);
    }
}
