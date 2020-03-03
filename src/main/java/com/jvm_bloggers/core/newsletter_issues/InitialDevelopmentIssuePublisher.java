package com.jvm_bloggers.core.newsletter_issues;

import com.jvm_bloggers.entities.blog.BlogType;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository;
import io.vavr.collection.List;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Profile;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.concurrent.CountDownLatch;

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

	static final int REQUIRED_AMOUNT_OF_POSTS = 10;
	static final int AMOUNT_OF_POSTS_TO_APPROVE = 2;
	private final NewNewsletterIssuePublisher newNewsletterIssuePublisher;
	private final NewsletterIssueRepository newsletterIssueRepository;
	private final BlogPostRepository blogPostRepository;
	private final PageRequest requiredAmountOfRecordsPageRequest = PageRequest.of(0, REQUIRED_AMOUNT_OF_POSTS);
	private final PageRequest amountOfRecordsToApprovePageRequest = PageRequest.of(0, AMOUNT_OF_POSTS_TO_APPROVE);

	@Scheduled(initialDelay = 10000, fixedDelayString = "${scheduler.publish-test-issue}")
	public void publishTestDevelopmentIssue() {
		log.info("Trying to publish a development issue if no exist...");
		if (!existsAnIssue() && existRequiredAmountOfPosts()) {
			log.info("PUBLISHED AN ISSUE");
			newNewsletterIssuePublisher.publishNewIssue(2);
		}
	}

	private boolean existsAnIssue() {
		boolean isExistAnIssue = newsletterIssueRepository.count() > 0;
		String message = isExistAnIssue ?
						"There already is an issue published. Aborting publishing of dev issue." :
						"There isn't an issue published.";
		log.info(message);
		return isExistAnIssue;
	}

	private boolean existRequiredAmountOfPosts() {
		log.info("Checking if enough posts exist.");
		boolean isRequiredAmountOfPosts = existRequiredAmountOfPostsOfType(BlogType.PERSONAL)
						&& existRequiredAmountOfPostsOfType(BlogType.COMPANY)
						&& existRequiredAmountOfPostsOfType(BlogType.VIDEOS);
		String message = isRequiredAmountOfPosts ?
						"There are enough posts for a dev issue." : "There aren't enough posts for a dev issue.";
		log.info(message);
		return isRequiredAmountOfPosts;
	}

	//TODO: Limit amount of found blog posts to the required amount
	private boolean existRequiredAmountOfPostsOfType(BlogType blogType) {
		List<BlogPost> blogPosts =
						blogPostRepository.findBlogPostsOfType(blogType, requiredAmountOfRecordsPageRequest);
		return blogPosts.size() >= REQUIRED_AMOUNT_OF_POSTS;
	}
}
