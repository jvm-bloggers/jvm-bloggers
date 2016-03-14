package pl.tomaszdziurko.jvm_bloggers.blog_posts;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.TimeConstants;

@Component
@Slf4j
public class BlogPostsFetchingScheduler {

    private final BlogPostsFetcher blogPostsFetcher;

    @Autowired
    public BlogPostsFetchingScheduler(BlogPostsFetcher blogPostsFetcher) {
        this.blogPostsFetcher = blogPostsFetcher;
    }

    @Scheduled(cron = TimeConstants.EVERY_SIX_HOURS)
    public void checkRssForNewBlogPosts() {
        log.info("Starting scheduler: fetching blog posts");
        blogPostsFetcher.refreshPosts();
    }
}
