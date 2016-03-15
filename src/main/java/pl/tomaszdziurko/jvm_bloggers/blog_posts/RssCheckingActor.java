package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import akka.actor.AbstractActor;
import akka.actor.ActorRef;
import akka.actor.Props;
import akka.japi.pf.ReceiveBuilder;

import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;

import lombok.extern.slf4j.Slf4j;

import pl.tomaszdziurko.jvm_bloggers.utils.SyndFeedProducer;

import java.util.List;

@Slf4j
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

    @SuppressWarnings("unchecked")
    private void executeAction(ActorRef postStoringActor, SyndFeedProducer syndFeedFactory,
                               RssLink rssLink) {
        try {
            SyndFeed feed = syndFeedFactory.createFor(rssLink.getUrl());
            List<SyndEntry> posts = feed.getEntries();
            posts.forEach(post -> {
                    RssEntryWithAuthor msg = new RssEntryWithAuthor(rssLink.getOwner(), post);
                    postStoringActor.tell(msg, self());
                }
            );
        } catch (Exception exception) {
            log.error("Exception for rss from {} ({}): {} ", rssLink.getOwner().getAuthor(),
                rssLink.getUrl(), exception.getMessage());
        }
    }
}
