package pl.tomaszdziurko.jvm_bloggers;

public class TimeConstants {

    public static final long FOUR_MINUTES = 4 * 60 * 1000L;

    public static final String EVERY_FRIDAY_AT_12_OCLOCK = "0 0 12 * * FRI";

    public static final String EVERY_TWO_HOURS_EXCLUDING_NIGHTS =
        "0 0 7,9,11,13,15,17,19,21,23 * * *";

    public static final String THIRTY_MINUTES_PAST_EVERY_HOUR = "0 30 * * * *";
}
