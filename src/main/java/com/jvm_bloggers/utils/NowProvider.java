package com.jvm_bloggers.utils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;

public interface NowProvider {

    LocalDateTime now();

    LocalDate today();
}
