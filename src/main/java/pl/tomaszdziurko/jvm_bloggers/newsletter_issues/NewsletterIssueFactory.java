package pl.tomaszdziurko.jvm_bloggers.newsletter_issues;

import lombok.AllArgsConstructor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;
import pl.tomaszdziurko.jvm_bloggers.mailing.IssueNumberRetriever;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataKeys;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataRepository;
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

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
