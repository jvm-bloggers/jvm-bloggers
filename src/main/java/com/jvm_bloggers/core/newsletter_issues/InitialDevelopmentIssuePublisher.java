package com.jvm_bloggers.core.newsletter_issues;

import com.jvm_bloggers.entities.blog.BlogType;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository;
import com.jvm_bloggers.utils.NowProvider;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Profile;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.List;

import static com.jvm_bloggers.ApplicationProfiles.DEV;
import static com.jvm_bloggers.core.newsletter_issues.PublishingConstants.DAYS_IN_THE_PAST_TO_INCLUDE_IN_NEW_ISSUE;
import static com.jvm_bloggers.entities.blog.BlogType.*;

@Component
@Slf4j
@Profile(DEV)
@RequiredArgsConstructor
public class InitialDevelopmentIssuePublisher {

    static final int REQUIRED_NUMBER_OF_POSTS = 3;
    static final int AMOUNT_OF_POSTS_TO_APPROVE = 2;
    private final PageRequest amountOfRecordsToApprovePageRequest = PageRequest.of(0, AMOUNT_OF_POSTS_TO_APPROVE);

    private final NowProvider nowProvider;
    private final NewNewsletterIssuePublisher newNewsletterIssuePublisher;
    private final NewsletterIssueRepository newsletterIssueRepository;
    private final BlogPostRepository blogPostRepository;

    @Scheduled(initialDelay = 5000, fixedDelayString = "${scheduler.publish-test-issue}")
    public void publishTestDevelopmentIssue() {
        if (thereAreNoIssuesPublished() && databaseContainsEnoughPostsForOneIssue()) {
            findAndApproveUnapprovedBlogPosts();
            newNewsletterIssuePublisher.publishNewIssue(DAYS_IN_THE_PAST_TO_INCLUDE_IN_NEW_ISSUE);
            log.info("Published a developer issue");
        }
    }

    private void findAndApproveUnapprovedBlogPosts() {
        List<BlogPost> unapprovedPodcastBlogPosts = blogPostRepository
                .findUnapprovedPostsByBlogType(PODCAST, amountOfRecordsToApprovePageRequest);
        List<BlogPost> unapprovedPresentationBlogPosts = blogPostRepository
                .findUnapprovedPostsByBlogType(PRESENTATION, amountOfRecordsToApprovePageRequest);
        List<BlogPost> unapprovedCompanyBlogPosts = blogPostRepository
                .findUnapprovedPostsByBlogType(COMPANY, amountOfRecordsToApprovePageRequest);
        if (unapprovedCompanyBlogPosts.size() > 0
                || unapprovedPodcastBlogPosts.size() > 0
                || unapprovedPresentationBlogPosts.size() > 0) {
            log.info("Approving posts for dev issue.");
            unapprovedPodcastBlogPosts.forEach(blogPost -> blogPost.approve(nowProvider.now()));
            unapprovedPresentationBlogPosts.forEach(blogPost -> blogPost.approve(nowProvider.now()));
            unapprovedCompanyBlogPosts.forEach(blogPost -> blogPost.approve(nowProvider.now()));
        } else {
            log.info("No unapproved posts found.");
        }
    }

    private boolean thereAreNoIssuesPublished() {
        boolean thereAreNoIssuesPublished = newsletterIssueRepository.count() == 0;
        if (thereAreNoIssuesPublished) {
            log.info("There are no published issues.");
        }
        return thereAreNoIssuesPublished;
    }

    private boolean databaseContainsEnoughPostsForOneIssue() {
        log.info("Checking if enough posts exist.");
        boolean enoughPostsExistInDatabase = databaseContainsEnoughBlogPostsOfType(PERSONAL)
                && databaseContainsEnoughBlogPostsOfType(COMPANY)
                && databaseContainsEnoughBlogPostsOfType(PODCAST)
                && databaseContainsEnoughBlogPostsOfType(PRESENTATION);
        if (enoughPostsExistInDatabase) {
            log.info("There are enough posts for a dev issue.");
        } else {
            log.info("There aren't enough posts for a dev issue.");
        }
        return enoughPostsExistInDatabase;
    }

    private boolean databaseContainsEnoughBlogPostsOfType(BlogType blogType) {
        List<BlogPost> blogPosts = blogPostRepository
                .findBlogPostsOfType(blogType, PageRequest.of(0, REQUIRED_NUMBER_OF_POSTS));
        return blogPosts.size() >= REQUIRED_NUMBER_OF_POSTS;
    }

}
