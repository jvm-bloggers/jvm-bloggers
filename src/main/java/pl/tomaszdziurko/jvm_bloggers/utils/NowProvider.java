package pl.tomaszdziurko.jvm_bloggers.utils;


import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneId;

@Component
public class NowProvider {

    public static final String ZONE_NAME = "Europe/Paris";

    public LocalDateTime now() {
        return LocalDateTime.now(ZoneId.of(ZONE_NAME));
    }
}
