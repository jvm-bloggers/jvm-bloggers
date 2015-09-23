package pl.tomaszdziurko.jvm_bloggers;

public class TimeConstants {

    public static final long ONE_MINUTE = 60 * 1000L;
    public static final long THIRTY_MINUTES = 30 * ONE_MINUTE;
    public static final long ONE_HOUR = 60 * ONE_MINUTE;

    public static final String EVERY_FRIDAY_AT_12_OCLOCK = "0 0 12 * * FRI";
    public static final String EVERY_FULL_HOUR_10_23 = "0 0 10-23 * * *";
    public static final String EVERY_HALF_HOUR_9_22 = "0 30 10-22 * * *";
}
