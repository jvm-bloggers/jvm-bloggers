package com.jvm_bloggers.core.data_fetching.blog_posts;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.routing.RoundRobinPool;

import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPostRepository;
import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog;
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogRepository;
import com.jvm_bloggers.core.metadata.Metadata;
import com.jvm_bloggers.core.metadata.MetadataKeys;
import com.jvm_bloggers.core.metadata.MetadataRepository;
import com.jvm_bloggers.core.rss.SyndFeedProducer;
import com.jvm_bloggers.utils.NowProvider;

import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

@Component
@Slf4j
public class BlogPostsFetcher {

    private final BlogRepository blogRepository;
    private final ActorRef rssCheckingActor;
    private final MetadataRepository metadataRepository;
    private final NowProvider nowProvider;

    private static final ThreadPoolExecutor
        fetchingBlogPostExecutor = new ThreadPoolExecutor(1, 1,
        0L, TimeUnit.MILLISECONDS,
        new LinkedBlockingQueue<>());

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
        if (isFetchingProcessInProgress()) {
            log.info("Fetching blog posts data already in progress");
            return;
        }
        fetchingBlogPostExecutor
            .submit(this::startFetchingProcess);
    }

    public void startFetchingProcess() {
        List<Blog> people = blogRepository.findAllActiveBlogs();
        people.stream()
            .filter(Blog::isActive)
            .forEach(person -> rssCheckingActor.tell(new RssLink(person), ActorRef.noSender()));

        final Metadata dateOfLastFetch = metadataRepository
            .findByName(MetadataKeys.DATE_OF_LAST_FETCHING_BLOG_POSTS);
        dateOfLastFetch.setValue(nowProvider.now().toString());
        metadataRepository.save(dateOfLastFetch);
    }

    public boolean isFetchingProcessInProgress() {
        return fetchingBlogPostExecutor.getActiveCount() != 0;
    }
}
