package com.jvm_bloggers.core.blogpost_redirect.click_counter;

import akka.actor.AbstractActor;
import akka.actor.Props;
import akka.japi.pf.ReceiveBuilder;
import com.jvm_bloggers.entities.click.Click;
import com.jvm_bloggers.entities.click.ClickRepository;
import com.jvm_bloggers.utils.NowProvider;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ClicksStoringActor extends AbstractActor {

    public ClicksStoringActor(ClickRepository clickRepository, NowProvider nowProvider) {
        log.debug("Creating " + ClicksStoringActor.class.getSimpleName());
        receive(ReceiveBuilder.match(SingleClick.class,
            clickEvent -> {
                log.debug("Storing click for " + clickEvent.getBlogPost().getUrl());
                clickRepository.save(new Click(clickEvent.getBlogPost(), nowProvider.now()));
            }).build()
        );
    }

    public static Props props(ClickRepository clickRepository, NowProvider nowProvider) {
        return Props.create(ClicksStoringActor.class, () -> {
                return new ClicksStoringActor(clickRepository, nowProvider);
            }
        );
    }
}
