package pl.tomaszdziurko.jvm_bloggers.utils;

import java.time.DayOfWeek;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;

import static pl.tomaszdziurko.jvm_bloggers.utils.NowProvider.DEFAULT_ZONE;

public class DateTimeUtilities {

    public static final DateTimeFormatter DATE_FORMATTER =
        DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm");

    public static LocalDateTime toLocalDateTime(Date date) {
        return date.toInstant().atZone(DEFAULT_ZONE).toLocalDateTime();
    }

    public static Date toDate(LocalDateTime localDateTime) {
        return Date.from(localDateTime.atZone(DEFAULT_ZONE).toInstant());
    }

    public static int daysBetweenDateAndLastFriday(LocalDateTime date) {
        int dateDayOfWeek = date.getDayOfWeek().getValue();
        int fridayDayOfWeek = DayOfWeek.FRIDAY.getValue();

        if (dateDayOfWeek > fridayDayOfWeek) {
            return dateDayOfWeek - fridayDayOfWeek;
        } else {
            return 7 - (fridayDayOfWeek - dateDayOfWeek);
        }
    }

    public static LocalDateTime lastPublicationDate(LocalDateTime currentDate) {
        int daysSinceLastFriday = daysBetweenDateAndLastFriday(currentDate);
        return currentDate.minusDays(daysSinceLastFriday).withHour(12).withMinute(0);
    }
}
