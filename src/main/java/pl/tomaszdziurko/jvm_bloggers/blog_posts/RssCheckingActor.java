package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import akka.actor.AbstractActor;
import akka.actor.ActorRef;
import akka.actor.Props;
import akka.japi.pf.ReceiveBuilder;
import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.io.SyndFeedInput;
import com.sun.syndication.io.XmlReader;
import lombok.extern.slf4j.Slf4j;

import java.net.URL;
import java.util.List;

@Slf4j
public class RssCheckingActor extends AbstractActor {

    public RssCheckingActor(ActorRef postStoringActor) {
        SyndFeedInput input = new SyndFeedInput();

        receive(ReceiveBuilder.match(RssLink.class, rssLink -> {
                SyndFeed feed = input.build(new XmlReader(new URL(rssLink.getUrl())));
                feed.getEntries().size();
                List<SyndEntry> posts = feed.getEntries();
                posts.forEach(post -> {
                        RssEntryWithAuthor msg = new RssEntryWithAuthor(rssLink.getOwner(), post);
                        postStoringActor.tell(msg, self());
                    }
                );
            }
        ).build());
    }

    public static Props props(ActorRef postStoringActor) {
        return Props.create(RssCheckingActor.class, () -> new RssCheckingActor(postStoringActor));
    }
}
