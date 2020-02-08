package com.jvm_bloggers.core.newsletter_issues;

import static com.jvm_bloggers.core.newsletter_issues.PublishingConstants.STARTING_HOUR_TO_INCLUDE_IN_NEW_ISSUE;
import static com.jvm_bloggers.entities.metadata.MetadataKeys.HEADING_TEMPLATE;
import static com.jvm_bloggers.entities.metadata.MetadataKeys.VARIA_TEMPLATE;

import com.jvm_bloggers.core.mailing.IssueNumberRetriever;
import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.entities.metadata.MetadataRepository;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import com.jvm_bloggers.utils.NowProvider;

import lombok.AllArgsConstructor;

import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

@Component
@AllArgsConstructor(onConstructor_ = {@Autowired})
@NoArgsConstructor
public class NewsletterIssueFactory {

    private IssueNumberRetriever issueNumberRetriever;
    private NowProvider nowProvider;
    private BlogRepository blogRepository;
    private BlogPostRepository blogPostRepository;
    private MetadataRepository metadataRepository;

    public NewsletterIssue create(int daysInThePastToIncludeInNewIssue, long issueNumber) {

        LocalDateTime startDate = calculateStartDate(daysInThePastToIncludeInNewIssue);
        List<Blog> newBlogs = blogRepository.findByDateAddedAfter(startDate).toJavaList();
        List<BlogPost> newApprovedPosts = blogPostRepository
            .findByApprovedDateAfterAndApprovedTrueOrderByApprovedDateAsc(startDate)
            .stream()
            .sorted(Comparator.comparing(blogPost -> blogPost.getBlog().getAuthor()))
            .collect(Collectors.toList());

        return NewsletterIssue.builder()
            .issueNumber(issueNumber)
            .publishedDate(nowProvider.today())
            .newBlogs(newBlogs)
            .blogPosts(newApprovedPosts)
            .heading(metadataRepository.findByName(HEADING_TEMPLATE).getValue())
            .varia(metadataRepository.findByName(VARIA_TEMPLATE).getValue())
            .build();
    }

    public NewsletterIssue create(int daysInThePastToIncludeInNewIssue) {
        long nextIssueNumber = issueNumberRetriever.getNextIssueNumber();
        return create(daysInThePastToIncludeInNewIssue, nextIssueNumber);
    }

    private LocalDateTime calculateStartDate(int daysInThePastToIncludeInNewIssue) {
        return nowProvider.now()
          .minusDays(daysInThePastToIncludeInNewIssue)
          .withHour(STARTING_HOUR_TO_INCLUDE_IN_NEW_ISSUE)
          .withMinute(0)
          .withSecond(0)
          .withNano(0);
    }
}
