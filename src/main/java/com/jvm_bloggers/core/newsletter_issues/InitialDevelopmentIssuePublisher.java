package com.jvm_bloggers.core.newsletter_issues;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository;
import io.vavr.concurrent.Future;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.wicket.request.resource.ContextRelativeResourceReference;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.annotation.Profile;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.ContextStartedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

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

    private final NewNewsletterIssuePublisher newNewsletterIssuePublisher;
    private final NewsletterIssueRepository newsletterIssueRepository;
    private final BlogPostRepository blogPostRepository;

    @EventListener(ApplicationReadyEvent.class)
    public void publishTestDevelopmentIssue(){
        log.debug("Publishing test issue");
        log.debug("Published test issue");
    }

    private boolean existsAnIssue(){
        return newsletterIssueRepository.count() > 0;
    }

    private boolean existPostsOfBlogTypeCompany(){
        return true;
    }
    private boolean existPostsOfBlogTypeVideo(){
        return true;
    }
}
