package com.jvm_bloggers.core.newsletter_issues;

import com.jvm_bloggers.entities.blog.BlogType;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository;
import io.vavr.collection.List;
import io.vavr.control.Try;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.validator.cfg.defs.pl.REGONDef;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.annotation.Profile;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

@Component
@Slf4j
@Profile("dev")
@RequiredArgsConstructor
public class InitialDevelopmentIssuePublisher {
// TODO
//1.  Sprawdzam czy istnieje już jakiś issue
//2.  Metoda czeka aż do bazy zostaną załadowane przykładowe dane
//    *   X blog postów z każdej kategorii
//    *   Scheduler? waitUntil?
//            3.  Ustawi varia i metadane
//4.  Zrobić approve kilku postom z kategorii VIDEO i COMPANY

    private final CountDownLatch blogPostCountLatch = new CountDownLatch(1);
    private final NewNewsletterIssuePublisher newNewsletterIssuePublisher;
    private final NewsletterIssueRepository newsletterIssueRepository;
    private final BlogPostRepository blogPostRepository;
    private final int REQUIRED_AMOUNT_OF_POSTS = 10;

    @EventListener(ApplicationReadyEvent.class)
    public void publishTestDevelopmentIssue() {
        if (!existsAnIssue()) {
            Try.run(this::waitForEnoughPostsToExistAndCreateDevIssue)
                    .onFailure(ex -> log.error("Waiting for blog posts interrupted" + ex));
        }
    }

    private void waitForEnoughPostsToExistAndCreateDevIssue() throws InterruptedException {
        log.info("Checking if enough posts exist");
        startPostAmountCheckerThread();
        log.info("Awaiting results");
        if (blogPostCountLatch.await(10, TimeUnit.SECONDS)) {
            blogPostCountLatch.countDown();
            log.debug("Publishing test issue");
            newNewsletterIssuePublisher.publishNewIssue(2);
            log.debug("Published test issue");
        } else {
            blogPostCountLatch.countDown();
            log.error("Couldn't get required amount of posts to create a Dev issue : " + REQUIRED_AMOUNT_OF_POSTS);
        }
    }

    private void startPostAmountCheckerThread() {
        Thread postAmountChecker = new Thread(this::existEnoughPosts);
        postAmountChecker.start();
    }

    private boolean existsAnIssue() {
        return newsletterIssueRepository.count() > 0;
    }

    private void existEnoughPosts() {
        log.info("Starting loop");
        while (!existRequiredAmountOfPosts() || blogPostCountLatch.getCount() > 0) {
            Try.run(() -> Thread.sleep(1000))
                    .onFailure(ex -> log.error("Waiting for blog posts interrupted" + ex));
        }
        log.info("Found enough posts, proceeding");
        blogPostCountLatch.countDown();
    }

    private boolean existRequiredAmountOfPosts() {
        return existRequiredAmountOfPostsOfType(BlogType.PERSONAL)
                && existRequiredAmountOfPostsOfType(BlogType.COMPANY)
                && existRequiredAmountOfPostsOfType(BlogType.VIDEOS);
    }

    private boolean existRequiredAmountOfPostsOfType(BlogType blogType) {
        List<BlogPost> blogPosts = blogPostRepository.findBlogPostsOfType(blogType);
        return blogPosts.size() > REQUIRED_AMOUNT_OF_POSTS;
    }
}
