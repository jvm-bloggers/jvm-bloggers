package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import com.google.common.base.Preconditions;
import com.sun.syndication.feed.synd.SyndEntry;
import lombok.Getter;
import pl.tomaszdziurko.jvm_bloggers.people.domain.Person;

@Getter
public class RssEntryWithAuthor {

    private final Person author;
    private final SyndEntry rssEntry;

    public RssEntryWithAuthor(Person author, SyndEntry rssEntry) {
        Preconditions.checkArgument(author != null, "Author can not be bull");
        Preconditions.checkArgument(rssEntry != null, "Rss entry can not be null");
        this.author = author;
        this.rssEntry = rssEntry;
    }

}
