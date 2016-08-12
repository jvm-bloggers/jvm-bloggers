package pl.tomaszdziurko.jvm_bloggers.click_counter;

import akka.actor.AbstractActor;
import akka.actor.Props;
import akka.japi.pf.ReceiveBuilder;

import lombok.extern.slf4j.Slf4j;

import pl.tomaszdziurko.jvm_bloggers.click_counter.domain.Click;
import pl.tomaszdziurko.jvm_bloggers.click_counter.domain.ClickRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

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
