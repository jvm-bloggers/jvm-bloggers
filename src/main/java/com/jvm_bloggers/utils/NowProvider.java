package com.jvm_bloggers.utils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;

public interface NowProvider {

    String DEFAULT_ZONE_NAME = "Europe/Warsaw";
    ZoneId DEFAULT_ZONE = ZoneId.of(DEFAULT_ZONE_NAME);

    LocalDateTime now();

    LocalDate today();
}
