package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.routing.RoundRobinPool;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.SyndFeedProducer;

import java.util.List;

import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.PRODUCTION;
import static pl.tomaszdziurko.jvm_bloggers.ApplicationProfiles.STAGE;

@Profile({PRODUCTION, STAGE})
@Component
public class ActorBlogPostsFetcher implements BlogPostsFetcher {

    private final BlogRepository blogRepository;
    private final ActorRef rssCheckingActor;
    private final ActorRef blogPostStoringActor;

    @Autowired
    public ActorBlogPostsFetcher(ActorSystem actorSystem, BlogRepository blogRepository,
                                 BlogPostRepository blogPostRepository,
                                 SyndFeedProducer syndFeedFactory) {
        this.blogRepository = blogRepository;
        blogPostStoringActor =
            actorSystem.actorOf(NewBlogPostStoringActor.props(blogPostRepository));
        rssCheckingActor = actorSystem.actorOf(new RoundRobinPool(10)
            .props(RssCheckingActor.props(blogPostStoringActor, syndFeedFactory)), "rss-checkers");
    }

    public void refreshPosts() {
        List<Blog> people = blogRepository.findAll();
        people.stream()
            .forEach(person -> rssCheckingActor.tell(new RssLink(person), ActorRef.noSender()));
    }
}
