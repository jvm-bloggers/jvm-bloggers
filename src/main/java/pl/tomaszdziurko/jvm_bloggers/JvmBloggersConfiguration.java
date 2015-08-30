package pl.tomaszdziurko.jvm_bloggers;


import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.routing.RoundRobinPool;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.RssCheckingActor;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.RssLink;

@Configuration
public class JvmBloggersConfiguration {

    @Bean
    public ActorSystem getActorSystem() {
        ActorSystem system = ActorSystem.create("jvm-bloggers-akka");
        configureListeners(system);
        return system;
    }

    private void configureListeners(ActorSystem actorSystem) {
        ActorRef actorRef = actorSystem.actorOf(RssCheckingActor
            .props()
            .withRouter(new RoundRobinPool(10))
            .create(RssCheckingActor.class), "rss-checker");
        actorSystem.eventStream().subscribe(actorRef, RssLink.class);
    }

}
