package com.jvm_bloggers.core;

import com.jvm_bloggers.core.data_fetching.blog_posts.BlogPostsFetchingScheduler;
import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPostRepository;
import com.jvm_bloggers.core.data_fetching.blogs.BloggersDataFetchingScheduler;
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogRepository;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * Initializes database with bloggers and posts data if database is empty.
 * Usually for the first time application starts.
 *
 * @author Marcin Kłopotek
 */
@Component
@Profile("!test")
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
@Slf4j
public class InitialBlogDataPopulationTrigger {

    private final BlogRepository blogRepository;
    private final BlogPostRepository blogPostRepository;
    private final BloggersDataFetchingScheduler bloggersDataFetchingScheduler;
    private final BlogPostsFetchingScheduler blogPostsFetchingScheduler;

    @PostConstruct
    public void initializeDatabaseWithBlogDataIfEmpty() {

        if (blogRepository.count() == 0) {
            log.info("No blogger entries found - populating database with data");
            bloggersDataFetchingScheduler.fetchBloggersData();
        }

        if (blogPostRepository.count() == 0) {
            log.info("No blog post entries found - populating database with data");
            blogPostsFetchingScheduler.checkRssForNewBlogPosts();
        }

    }

}
