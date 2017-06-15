package com.jvm_bloggers.core.data_fetching.blog_posts;

import akka.actor.AbstractActor;
import akka.actor.ActorRef;
import akka.actor.Props;
import com.jvm_bloggers.core.rss.SyndFeedProducer;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class RssCheckingActor extends AbstractActor {

    private final ActorRef postStoringActor;
    private final SyndFeedProducer syndFeedFactory;

    @Override
    public Receive createReceive() {
        return receiveBuilder().create().match(RssLink.class,
            rssLink -> executeAction(postStoringActor, syndFeedFactory, rssLink)
        ).build();
    }

    public static Props props(ActorRef postStoringActor, SyndFeedProducer syndFeedFactory) {
        return Props.create(RssCheckingActor.class,
            () -> new RssCheckingActor(postStoringActor, syndFeedFactory));
    }

    private void executeAction(ActorRef postStoringActor, SyndFeedProducer syndFeedFactory,
                               RssLink rssLink) {
        syndFeedFactory.createFor(rssLink.getUrl()).forEach(feed ->
            feed.getEntries().forEach(post -> {
                RssEntryWithAuthor msg = new RssEntryWithAuthor(rssLink.getOwner(), post);
                postStoringActor.tell(msg, self());
            })
        );
    }

}
