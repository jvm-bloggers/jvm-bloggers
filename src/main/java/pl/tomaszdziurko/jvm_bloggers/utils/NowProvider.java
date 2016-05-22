package pl.tomaszdziurko.jvm_bloggers.utils;


import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;

@Component
public class NowProvider {

    public static final ZoneId DEFAULT_ZONE = ZoneId.of("Europe/Warsaw");

    public LocalDateTime now() {
        return LocalDateTime.now(DEFAULT_ZONE);
    }

    public LocalDate today() {
        return now().toLocalDate();
    }
}
