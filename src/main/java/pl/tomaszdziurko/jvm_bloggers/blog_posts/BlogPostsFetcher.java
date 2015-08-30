package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import akka.actor.ActorSystem;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.people.Person;
import pl.tomaszdziurko.jvm_bloggers.people.PersonRepository;

import java.util.List;

@Component
public class BlogPostsFetcher {

    ActorSystem actorSystem;
    PersonRepository personRepository;

    @Autowired
    public BlogPostsFetcher(ActorSystem actorSystem, PersonRepository personRepository) {
        this.actorSystem = actorSystem;
        this.personRepository = personRepository;
    }

    public void refreshPosts() {
        List<Person> people = personRepository.findAll();
        people.stream().forEach(person -> actorSystem.eventStream().publish(new RssLink(person)));
    }
}
