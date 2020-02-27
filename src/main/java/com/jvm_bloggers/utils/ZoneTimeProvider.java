package com.jvm_bloggers.utils;

import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Component
public final class ZoneTimeProvider implements NowProvider {

    @Override
    public LocalDateTime now() {
        return LocalDateTime.now(DEFAULT_ZONE);
    }

    @Override
    public LocalDate today() {
        return now().toLocalDate();
    }
}
