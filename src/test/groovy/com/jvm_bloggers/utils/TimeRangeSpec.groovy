package com.jvm_bloggers.utils


import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalTime

@Subject(TimeRange)
class TimeRangeSpec extends Specification {

    def "Should throw if start time is after end time"() {
        when:
        TimeRange.between(LocalTime.of(12, 0), LocalTime.of(11, 0))

        then:
        IllegalArgumentException e = thrown(IllegalArgumentException)
        e.message == 'End time must be >= start time'
    }

    @Unroll
    def "Should return true if given time is contained in range"() {
        given:
        TimeRange range = TimeRange.between(LocalTime.of(0, 0), LocalTime.of(8, 0))

        expect:
        range.contains(time) == inRange

        where:
        time                 || inRange
        LocalTime.of(0, 0)   || true
        LocalTime.of(8, 0)   || true
        LocalTime.of(23, 59) || false
        LocalTime.of(8, 1)   || false
    }

}
