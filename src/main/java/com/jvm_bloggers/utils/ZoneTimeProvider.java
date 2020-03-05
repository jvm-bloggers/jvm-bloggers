package com.jvm_bloggers.utils;

import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;

@Component
public final class ZoneTimeProvider implements NowProvider {

    public static final String DEFAULT_ZONE_NAME = "Europe/Warsaw";
    public static final ZoneId DEFAULT_ZONE = ZoneId.of(DEFAULT_ZONE_NAME);

    @Override
    public LocalDateTime now() {
        return LocalDateTime.now(DEFAULT_ZONE);
    }

    @Override
    public LocalDate today() {
        return now().toLocalDate();
    }
}
