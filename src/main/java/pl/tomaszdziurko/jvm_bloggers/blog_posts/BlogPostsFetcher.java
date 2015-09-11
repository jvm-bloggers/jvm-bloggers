package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.routing.RoundRobinPool;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.people.Person;
import pl.tomaszdziurko.jvm_bloggers.people.PersonRepository;

import java.util.List;

@Component
public class BlogPostsFetcher {

    private PersonRepository personRepository;
    private ActorRef rssCheckingActor;
    private ActorRef blogPostStoringActor;

    @Autowired
    public BlogPostsFetcher(ActorSystem actorSystem, PersonRepository personRepository) {
        this.personRepository = personRepository;
        blogPostStoringActor = actorSystem.actorOf(NewBlogPostStoringActor.props().create(NewBlogPostStoringActor.class));
        rssCheckingActor = actorSystem.actorOf(new RoundRobinPool(10).props(RssCheckingActor.props(blogPostStoringActor)), "rss-checkers");
    }

    public void refreshPosts() {
        List<Person> people = personRepository.findAll();
        people.stream().forEach(person -> rssCheckingActor.tell(new RssLink(person), ActorRef.noSender()));
    }
}
