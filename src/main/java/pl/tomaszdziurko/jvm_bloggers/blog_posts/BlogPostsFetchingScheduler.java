package pl.tomaszdziurko.jvm_bloggers.blog_posts;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.TimeConstants;
import pl.tomaszdziurko.jvm_bloggers.settings.Metadata;
import pl.tomaszdziurko.jvm_bloggers.settings.MetadataKeys;
import pl.tomaszdziurko.jvm_bloggers.settings.MetadataRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

@Component
@Slf4j
public class BlogPostsFetchingScheduler {

    private final BlogPostsFetcher blogPostsFetcher;
    private MetadataRepository metadataRepository;
    private NowProvider nowProvider;

    @Autowired
    public BlogPostsFetchingScheduler(BlogPostsFetcher blogPostsFetcher,
                                      MetadataRepository metadataRepository,
                                      NowProvider nowProvider) {
        this.blogPostsFetcher = blogPostsFetcher;
        this.metadataRepository = metadataRepository;
        this.nowProvider = nowProvider;
    }

    @Scheduled(cron = TimeConstants.EVERY_SIX_HOURS)
    public void checkRssForNewBlogPosts() {
        log.info("Starting scheduler: fetching blog posts");
        blogPostsFetcher.refreshPosts();
        final Metadata dateOfLastFetch = metadataRepository.findByName(MetadataKeys.DATE_OF_LAST_FETCHING_BLOG_POSTS.toString());
        dateOfLastFetch.setValue(nowProvider.now().toString());
        metadataRepository.save(dateOfLastFetch);
    }
}
