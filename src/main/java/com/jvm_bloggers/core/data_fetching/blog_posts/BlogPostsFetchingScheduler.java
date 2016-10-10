package com.jvm_bloggers.core.data_fetching.blog_posts;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class BlogPostsFetchingScheduler {

    private final BlogPostsFetcher blogPostsFetcher;

    @Scheduled(cron = "${scheduler.fetch-rss-for-new-blogs}")
    public void checkRssForNewBlogPosts() {
        log.info("Starting scheduler: fetching blog posts");
        blogPostsFetcher.refreshPosts();
    }
}
