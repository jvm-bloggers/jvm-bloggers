package com.jvm_bloggers.core.data_fetching.blog_posts;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.routing.RoundRobinPool;

import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPostRepository;
import com.jvm_bloggers.core.data_fetching.blogs.PreventConcurrentExecutionSafeguard;
import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog;
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogRepository;
import com.jvm_bloggers.core.metadata.Metadata;
import com.jvm_bloggers.core.metadata.MetadataKeys;
import com.jvm_bloggers.core.metadata.MetadataRepository;
import com.jvm_bloggers.core.rss.SyndFeedProducer;
import com.jvm_bloggers.utils.NowProvider;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class BlogPostsFetcher {

    private final BlogRepository blogRepository;
    private final ActorRef rssCheckingActor;
    private final MetadataRepository metadataRepository;
    private final NowProvider nowProvider;
    private final PreventConcurrentExecutionSafeguard concurrentExecutionSafeguard
        = new PreventConcurrentExecutionSafeguard();

    @Autowired
    public BlogPostsFetcher(ActorSystem actorSystem, BlogRepository blogRepository,
                            BlogPostRepository blogPostRepository,
                            BlogPostFactory blogPostFactory,
                            SyndFeedProducer syndFeedFactory,
                            MetadataRepository metadataRepository,
                            NowProvider nowProvider) {
        this.blogRepository = blogRepository;
        final ActorRef blogPostStoringActor =
            actorSystem.actorOf(NewBlogPostStoringActor.props(blogPostRepository, blogPostFactory));
        rssCheckingActor = actorSystem.actorOf(new RoundRobinPool(10)
            .props(RssCheckingActor.props(blogPostStoringActor, syndFeedFactory)), "rss-checkers");
        this.metadataRepository = metadataRepository;
        this.nowProvider = nowProvider;
    }

    public void refreshPosts() {
        concurrentExecutionSafeguard.preventConcurrentExecution(() -> startFetchingProcess());
    }

    @Async("singleThreadExecutor")
    public void refreshPostsAsynchronously() {
        refreshPosts();
    }

    private Void startFetchingProcess() {
        List<Blog> people = blogRepository.findAllActiveBlogs();
        people.stream()
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
