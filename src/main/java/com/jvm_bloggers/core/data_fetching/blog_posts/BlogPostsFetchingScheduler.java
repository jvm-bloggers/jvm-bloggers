package com.jvm_bloggers.core.data_fetching.blog_posts;

import com.jvm_bloggers.core.metadata.Metadata;
import com.jvm_bloggers.core.metadata.MetadataKeys;
import com.jvm_bloggers.core.metadata.MetadataRepository;
import com.jvm_bloggers.utils.NowProvider;

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
    private final MetadataRepository metadataRepository;
    private final NowProvider nowProvider;

    @Scheduled(cron = "${scheduler.fetch-rss-for-new-blogs}")
    public void checkRssForNewBlogPosts() {
        log.info("Starting scheduler: fetching blog posts");
        blogPostsFetcher.refreshPosts();
        final Metadata dateOfLastFetch = metadataRepository
                .findByName(MetadataKeys.DATE_OF_LAST_FETCHING_BLOG_POSTS);
        dateOfLastFetch.setValue(nowProvider.now().toString());
        metadataRepository.save(dateOfLastFetch);
    }
}
