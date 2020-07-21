package com.jvm_bloggers.core.data_fetching.blog_posts;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.routing.RoundRobinPool;
import com.jvm_bloggers.core.data_fetching.blogs.PreventConcurrentExecutionSafeguard;
import com.jvm_bloggers.core.rss.SyndFeedProducer;
import com.jvm_bloggers.entities.blog.Blog;
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
    private ActorRef rssCheckingActor;
    private MetadataRepository metadataRepository;
    private NowProvider nowProvider;
    private PreventConcurrentExecutionSafeguard concurrentExecutionSafeguard
            = new PreventConcurrentExecutionSafeguard();

    @Autowired
    public BlogPostsFetcher(ActorSystem actorSystem, BlogRepository blogRepository,
            BlogPostService blogPostService,
            SyndFeedProducer syndFeedFactory,
            MetadataRepository metadataRepository,
            NowProvider nowProvider) {
        this.blogRepository = blogRepository;
        final ActorRef blogPostStoringActor = actorSystem
                .actorOf(NewBlogPostStoringActor.props(blogPostService));
        rssCheckingActor = actorSystem.actorOf(new RoundRobinPool(10)
                .props(RssCheckingActor.props(blogPostStoringActor, syndFeedFactory)), "rss-checkers");
        this.metadataRepository = metadataRepository;
        this.nowProvider = nowProvider;
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
                .filter(Blog::isActive)
                .forEach(person -> rssCheckingActor.tell(new RssLink(person), ActorRef.noSender()));

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
