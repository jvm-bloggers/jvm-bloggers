package com.jvm_bloggers.core.data_fetching.blog_posts;

import com.jvm_bloggers.core.data_fetching.blogs.PreventConcurrentExecutionSafeguard;
import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.entities.metadata.Metadata;
import com.jvm_bloggers.entities.metadata.MetadataKeys;
import com.jvm_bloggers.entities.metadata.MetadataRepository;
import com.jvm_bloggers.utils.NowProvider;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

@Component
@NoArgsConstructor
public class BlogPostsFetcher {

    private BlogRepository blogRepository;
    private MetadataRepository metadataRepository;
    private SingleRssChecker singleRssChecker;
    private NowProvider nowProvider;
    private PreventConcurrentExecutionSafeguard concurrentExecutionSafeguard;

    @Autowired
    public BlogPostsFetcher(BlogRepository blogRepository,
            MetadataRepository metadataRepository,
            SingleRssChecker singleRssChecker,
            NowProvider nowProvider) {
        this.blogRepository = blogRepository;
        this.singleRssChecker = singleRssChecker;
        this.metadataRepository = metadataRepository;
        this.nowProvider = nowProvider;
        concurrentExecutionSafeguard = new PreventConcurrentExecutionSafeguard();
    }

    void refreshPosts() {
        concurrentExecutionSafeguard.preventConcurrentExecution(this::startFetchingProcess);
    }

    @Async("singleThreadExecutor")
    public void refreshPostsAsynchronously() {
        refreshPosts();
    }

    private Void startFetchingProcess() {
        blogRepository.findAllActiveBlogs()
                .shuffle()
                .forEach(
                        person -> singleRssChecker.checkForNewEntries(person)
                );

        final Metadata dateOfLastFetch = metadataRepository
                .findByName(MetadataKeys.DATE_OF_LAST_FETCHING_BLOG_POSTS);
        dateOfLastFetch.setValue(nowProvider.now().toString());
        metadataRepository.save(dateOfLastFetch);
        return null;
    }

    public boolean isFetchingProcessInProgress() {
        return concurrentExecutionSafeguard.isExecuting();
    }
}
