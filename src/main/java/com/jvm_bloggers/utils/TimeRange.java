package com.jvm_bloggers.utils;

import lombok.AccessLevel;
import lombok.Data;
import lombok.RequiredArgsConstructor;

import java.time.LocalTime;
import java.time.temporal.ChronoUnit;

@Data
@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class TimeRange {

    private final LocalTime startTime;
    private final LocalTime endTime;

    public static TimeRange between(LocalTime startTime, LocalTime endTime) {
        if (startTime.isAfter(endTime)) {
            throw new IllegalArgumentException("End time must be >= start time");
        }
        return new TimeRange(startTime, endTime);
    }

    public boolean contains(LocalTime time) {
        return ChronoUnit.NANOS.between(startTime, time) >= 0
                && ChronoUnit.NANOS.between(time, endTime) >= 0;
    }
}
