package com.jvm_bloggers.utils;

import lombok.experimental.UtilityClass;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Locale;

import static com.jvm_bloggers.utils.ZoneTimeProvider.DEFAULT_ZONE;
import static java.lang.String.format;
import static java.time.format.TextStyle.FULL_STANDALONE;

@UtilityClass
public class DateTimeUtilities {

    public static final int DAY_OF_MONTH_ENDING_SUMMARY_PERIOD = 11;

    private static final Locale POLISH_LOCALE = new Locale("PL");

    public static final DateTimeFormatter DATE_TIME_FORMATTER =
        DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm");

    public static final DateTimeFormatter DATE_FORMATTER =
        DateTimeFormatter.ofPattern("dd/MM/yyyy");

    public static final DateTimeFormatter YEAR_MONTH_FORMATTER =
        DateTimeFormatter.ofPattern("yyyy/MM");

    public static LocalDateTime toLocalDateTime(Date date) {
        return date.toInstant().atZone(DEFAULT_ZONE).toLocalDateTime();
    }

    public static Date toDate(LocalDateTime localDateTime) {
        return Date.from(localDateTime.atZone(DEFAULT_ZONE).toInstant());
    }

    public static Date toDate(LocalDate localDate) {
        return toDate(localDate.atStartOfDay());
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

    public static String stringify(YearMonth yearMonth) {
        String monthName = yearMonth.getMonth().getDisplayName(FULL_STANDALONE, POLISH_LOCALE);
        return format("%s %d", monthName, yearMonth.getYear());
    }

}
