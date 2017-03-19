package com.jvm_bloggers.domain.command;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class CommandPublisher {

    private final ApplicationEventPublisher eventPublisher;

    @Autowired
    public CommandPublisher(ApplicationEventPublisher eventPublisher) {
        this.eventPublisher = eventPublisher;
    }

    public void publish(Command command) {
        log.info("Received command {}", command);
        eventPublisher.publishEvent(command);
    }

}
