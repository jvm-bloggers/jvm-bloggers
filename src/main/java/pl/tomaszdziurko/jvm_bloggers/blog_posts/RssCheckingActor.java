package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import akka.actor.AbstractActor;
import akka.actor.ActorRef;
import akka.actor.Props;
import akka.japi.pf.ReceiveBuilder;

import com.rometools.rome.feed.synd.SyndEntry;

import pl.tomaszdziurko.jvm_bloggers.utils.SyndFeedProducer;

import java.util.List;

public class RssCheckingActor extends AbstractActor {

    public RssCheckingActor(ActorRef postStoringActor, SyndFeedProducer syndFeedFactory) {
        receive(ReceiveBuilder.match(RssLink.class, rssLink -> {
                executeAction(postStoringActor, syndFeedFactory, rssLink);
            }
        ).build());
    }

    public static Props props(ActorRef postStoringActor, SyndFeedProducer syndFeedFactory) {
        return Props.create(RssCheckingActor.class,
            () -> new RssCheckingActor(postStoringActor, syndFeedFactory));
    }

    private void executeAction(ActorRef postStoringActor, SyndFeedProducer syndFeedFactory,
                               RssLink rssLink) {
        syndFeedFactory.createFor(rssLink.getUrl()).ifPresent(feed -> {
                List<SyndEntry> posts = feed.getEntries();
                posts.forEach(post -> {
                    RssEntryWithAuthor msg = new RssEntryWithAuthor(rssLink.getOwner(), post);
                    postStoringActor.tell(msg, self());
                });
            }
        );
    }
}
