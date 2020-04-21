package com.jvm_bloggers.core.data_fetching.blog_posts;

import akka.actor.AbstractActor;
import akka.actor.ActorRef;
import akka.actor.Props;

import com.jvm_bloggers.core.rss.SyndFeedProducer;
import com.jvm_bloggers.utils.NowProvider;
import com.rometools.rome.feed.synd.SyndEntry;

import lombok.RequiredArgsConstructor;

import java.time.LocalDateTime;
import java.time.ZoneId;

@RequiredArgsConstructor
public class RssCheckingActor extends AbstractActor {

    private final ActorRef postStoringActor;
    private final SyndFeedProducer syndFeedFactory;
    private final NowProvider nowProvider;

    @Override
    public Receive createReceive() {
        return receiveBuilder().create().match(RssLink.class,
            rssLink -> executeAction(postStoringActor, syndFeedFactory, rssLink)
        ).build();
    }

    public static Props props(ActorRef postStoringActor, SyndFeedProducer syndFeedFactory,
                              NowProvider nowProvider) {
        return Props.create(RssCheckingActor.class,
            () -> new RssCheckingActor(postStoringActor, syndFeedFactory, nowProvider));
    }

    private void executeAction(ActorRef postStoringActor, SyndFeedProducer syndFeedFactory,
                               RssLink rssLink) {
        syndFeedFactory.createFor(rssLink.getUrl()).forEach(feed ->
            feed.getEntries().stream()
                .filter(this::isFromLastWeek)
                .forEach(post -> {
                    RssEntryWithAuthor msg = new RssEntryWithAuthor(rssLink.getOwner(), post);
                    postStoringActor.tell(msg, self());
                })
        );
    }

    private boolean isFromLastWeek(SyndEntry post) {
        return LocalDateTime.ofInstant(post.getPublishedDate().toInstant(), ZoneId.systemDefault())
            .compareTo(nowProvider.now().minusDays(7)) > 0;
    }

}
