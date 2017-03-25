package com.jvm_bloggers.core.utils

import com.jvm_bloggers.utils.NowProvider
import spock.lang.Specification
import spock.lang.Subject

import java.sql.Timestamp
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime

class LocalDateToTimestampConverterSpec extends Specification {

    @Subject
    LocalDateToTimestampConverter converter = new LocalDateToTimestampConverter()

    def "Should convert"() {
        given:
        LocalDateTime localDateTime = LocalDateTime.of(2016, 05, 7, 8, 44, 0)
        Timestamp timestamp = convertToTimestamp(localDateTime)

        when:
        LocalDate localDate = converter.convertToEntityAttribute(timestamp)

        then:
        localDate.getDayOfMonth() == localDateTime.getDayOfMonth()
        localDate.getMonthValue() == localDateTime.getMonthValue()
        localDate.getYear() == localDateTime.getYear()
    }

    private Timestamp convertToTimestamp(LocalDateTime localDateTime) {
        Instant instant = localDateTime.atZone(NowProvider.DEFAULT_ZONE).toInstant()
        Timestamp timestamp = Timestamp.from(instant)
        return timestamp
    }

}
