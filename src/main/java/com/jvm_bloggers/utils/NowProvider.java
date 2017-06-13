package com.jvm_bloggers.utils;


import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;

@Component
public class NowProvider {

    public static final String DEFAULT_ZONE_NAME = "Europe/Warsaw";
    public static final ZoneId DEFAULT_ZONE = ZoneId.of(DEFAULT_ZONE_NAME);

    public LocalDateTime now() {
        return LocalDateTime.now(DEFAULT_ZONE);
    }

    public LocalDate today() {
        return now().toLocalDate();
    }
}
