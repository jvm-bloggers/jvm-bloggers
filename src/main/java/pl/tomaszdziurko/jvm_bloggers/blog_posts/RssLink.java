package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import com.google.common.base.Preconditions;
import pl.tomaszdziurko.jvm_bloggers.people.domain.Person;

public class RssLink {

    private final Person person;

    public RssLink(Person person) {
        Preconditions.checkArgument(person != null, "Person can not be bull");
        Preconditions.checkArgument(person.getRss() != null, "Rss link can not be null");
        this.person = person;
    }

    public String getUrl() {
        return person.getRss();
    }

    public Person getOwner() {
        return person;
    }
}
