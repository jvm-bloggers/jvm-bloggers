package com.jvm_bloggers.core.data_fetching.blog_posts;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.routing.RoundRobinPool;

import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPostRepository;
import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog;
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogRepository;
import com.jvm_bloggers.core.rss.SyndFeedProducer;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class BlogPostsFetcher {

    private final BlogRepository blogRepository;
    private final ActorRef rssCheckingActor;
    private final ActorRef blogPostStoringActor;

    @Autowired
    public BlogPostsFetcher(ActorSystem actorSystem, BlogRepository blogRepository,
                            BlogPostRepository blogPostRepository,
                            SyndFeedProducer syndFeedFactory) {
        this.blogRepository = blogRepository;
        blogPostStoringActor =
            actorSystem.actorOf(NewBlogPostStoringActor.props(blogPostRepository));
        rssCheckingActor = actorSystem.actorOf(new RoundRobinPool(10)
            .props(RssCheckingActor.props(blogPostStoringActor, syndFeedFactory)), "rss-checkers");
    }

    public void refreshPosts() {
        List<Blog> people = blogRepository.findAllActiveBlogs();
        people.stream()
            .filter(Blog::isActive)
            .forEach(person -> rssCheckingActor.tell(new RssLink(person), ActorRef.noSender()));
    }
}
