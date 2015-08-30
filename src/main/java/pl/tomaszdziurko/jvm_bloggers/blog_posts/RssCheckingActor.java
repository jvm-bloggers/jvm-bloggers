package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import akka.actor.AbstractActor;
import akka.actor.Props;
import akka.japi.pf.ReceiveBuilder;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class RssCheckingActor extends AbstractActor {

    public RssCheckingActor() {
        receive(ReceiveBuilder.match(RssLink.class, rssLink ->
                log.info("Received " + rssLink.getUrl())
        ).build());
    }

    public static Props props() {
        return Props.create(RssCheckingActor.class, RssCheckingActor::new);
    }
}
