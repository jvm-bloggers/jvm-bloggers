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

    public RssCheckingActor(ActorRef postStoringActor, SyndFeedInput input) {
        receive(ReceiveBuilder.match(RssLink.class, rssLink -> {
                executeAction(postStoringActor, input, rssLink);
            }
        ).build());
    }

    @SuppressWarnings("unchecked")
    private void executeAction(ActorRef postStoringActor, SyndFeedInput input, RssLink rssLink) {
        try {
            SyndFeed feed = input.build(new XmlReader(new URL(rssLink.getUrl())));
            List<SyndEntry> posts = feed.getEntries();
            posts.forEach(post -> {
                    RssEntryWithAuthor msg = new RssEntryWithAuthor(rssLink.getOwner(), post);
                    postStoringActor.tell(msg, self());
                }
            );
        } catch (Exception e) {
            log.error("Exception for rss from {} ({}): {} ", rssLink.getOwner().getName(), rssLink.getUrl(), e.getMessage());
        }
    }

    public static Props props(ActorRef postStoringActor, SyndFeedInput input) {
        return Props.create(RssCheckingActor.class, () -> new RssCheckingActor(postStoringActor, input));
    }
}
