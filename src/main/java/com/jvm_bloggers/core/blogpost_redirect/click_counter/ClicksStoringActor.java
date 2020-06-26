package com.jvm_bloggers.core.blogpost_redirect.click_counter;

import akka.actor.AbstractActor;
import akka.actor.Props;
import com.jvm_bloggers.entities.click.Click;
import com.jvm_bloggers.entities.click.ClickRepository;
import com.jvm_bloggers.utils.NowProvider;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RequiredArgsConstructor
public class ClicksStoringActor extends AbstractActor {

    private final ClickRepository clickRepository;
    private final NowProvider nowProvider;

    @Override
    public Receive createReceive() {
        log.debug("Creating " + ClicksStoringActor.class.getSimpleName());
        return receiveBuilder().match(SingleClick.class,
            clickEvent -> {
                log.debug("Storing click for " + clickEvent.getBlogPost().getUrl());
                clickRepository.save(createClick(clickEvent));
            }).build();
    }

    private Click createClick(SingleClick clickEvent) {
        return new Click(
            clickEvent.getBlogPost(),
            nowProvider.now(),
            clickEvent.getIp().getValue(),
            clickEvent.getReferer().getValue(),
            clickEvent.getUserAgent().getValue()
        );
    }

    public static Props props(ClickRepository clickRepository, NowProvider nowProvider) {
        return Props.create(ClicksStoringActor.class, () ->
                new ClicksStoringActor(clickRepository, nowProvider)
        );
    }

}
