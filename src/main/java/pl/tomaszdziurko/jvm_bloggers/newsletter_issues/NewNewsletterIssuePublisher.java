package pl.tomaszdziurko.jvm_bloggers.newsletter_issues;


import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

import java.time.LocalDateTime;
import java.util.List;

@Component
@Slf4j
class NewNewsletterIssuePublisher {

    private final NowProvider nowProvider;
    private final BlogRepository blogRepository;
    private final BlogPostRepository blogPostRepository;
    private final NewsletterIssueService newsletterIssueService;
    private final ApplicationEventPublisher eventPublisher;

    @Autowired
    public NewNewsletterIssuePublisher(
        BlogPostRepository blogPostRepository,
        NowProvider nowProvider,
        BlogRepository blogRepository,
        NewsletterIssueService newsletterIssueService,
        ApplicationEventPublisher eventPublisher) {

        this.blogPostRepository = blogPostRepository;
        this.nowProvider = nowProvider;
        this.blogRepository = blogRepository;
        this.newsletterIssueService = newsletterIssueService;
        this.eventPublisher = eventPublisher;
    }

    void publishNewIssue(int daysInThePastToIncludeInNewIssue) {
        LocalDateTime startDate = calculateStartDate(daysInThePastToIncludeInNewIssue);
        List<Blog> newBlogs = blogRepository.findByDateAddedAfter(startDate);
        List<BlogPost> newApprovedPosts = blogPostRepository
            .findByPublishedDateAfterAndApprovedTrueOrderByPublishedDateAsc(startDate);
        NewsletterIssue newIssue = newsletterIssueService.saveNewIssue(
            newBlogs,
            newApprovedPosts
        );
        eventPublisher.publishEvent(new NewIssuePublished(newIssue));

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
