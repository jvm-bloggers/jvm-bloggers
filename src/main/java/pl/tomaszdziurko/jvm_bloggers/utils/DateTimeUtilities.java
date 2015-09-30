package pl.tomaszdziurko.jvm_bloggers.utils;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;

public class DateTimeUtilities {

    public static LocalDateTime convertDateToLocalDateTime(Date date) {
        return date.toInstant().atZone(ZoneId.of(NowProvider.ZONE_NAME)).toLocalDateTime();
    }
}
