package pl.tomaszdziurko.jvm_bloggers.utils

import spock.lang.Specification
import spock.lang.Unroll

import java.time.LocalDateTime

class DateTimeUtilitiesSpec extends Specification {

    @Unroll
    def 'Should calculate number of days since last friday for #date'() {

        when:
            int daysSinceLastFriday = DateTimeUtilities.daysBetweenDateAndLastFriday(date)
        then:
            daysSinceLastFriday == expectedNumberOfDays
        where:
            date                                  | expectedNumberOfDays
            LocalDateTime.of(2016, 2, 7, 10, 10)  | 2       // this is Sunday so expected value is 2
            LocalDateTime.of(2016, 2, 8, 10, 10)  | 3
            LocalDateTime.of(2016, 2, 9, 10, 10)  | 4
            LocalDateTime.of(2016, 2, 10, 10, 10) | 5
            LocalDateTime.of(2016, 2, 11, 10, 10) | 6
            LocalDateTime.of(2016, 2, 12, 10, 10) | 7
            LocalDateTime.of(2016, 2, 13, 10, 10) | 1
            LocalDateTime.of(2016, 2, 14, 10, 10) | 2
            LocalDateTime.of(2016, 2, 15, 10, 10) | 3
    }

    @Unroll
    def "should return #expectedDate for #date"(LocalDateTime date, LocalDateTime expectedDate) {
        given:
            def nowProviderMock = Mock(NowProvider)
            nowProviderMock.now() >> date
        when:
            LocalDateTime lastFriday = DateTimeUtilities.lastMailingDate(nowProviderMock)
        then:
            lastFriday == expectedDate
        where:
            date                                 | expectedDate
            LocalDateTime.of(2016, 3, 5, 19, 20) | LocalDateTime.of(2016, 3, 4, 12, 0)
            LocalDateTime.of(2016, 3, 4, 11, 20) | LocalDateTime.of(2016, 2, 26, 12, 0)

    }
}
