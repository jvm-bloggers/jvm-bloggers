package pl.tomaszdziurko.jvm_bloggers;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.BlogPostsFetchingScheduler;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.blogs.BloggersDataFetchingScheduler;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;

/**
 * Initializes database with bloggers and posts data if database is empty.
 * Usually for the first time application starts.
 * 
 */
@Component
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
