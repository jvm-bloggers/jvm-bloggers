package com.jvm_bloggers.core.data_fetching.blogs

import spock.lang.Specification
import spock.lang.Subject

import java.util.stream.Stream

@Subject(UpdateStatistic)
class UpdateStatisticSpec extends Specification {

    def "Should default to zero"() {
        given:
        Stream<UpdateStatus> emptyStream = Stream.of()

        when:
        UpdateStatistic testObj = emptyStream.collect(UpdateStatistic.collector())

        then:
        with(testObj) {
            getTotal() == 0
            getUpdated() == 0
            getCreated() == 0
            getInvalid() == 0
            getNotChanged() == 0
        }
    }

    def "Should count repeating statuses"() {
        given:
        Stream<UpdateStatus> emptyStream = Stream.of(
            UpdateStatus.CREATED,
            UpdateStatus.INVALID,
            UpdateStatus.NOT_CHANGED,
            UpdateStatus.NOT_CHANGED,
            UpdateStatus.NOT_CHANGED,
            UpdateStatus.CREATED,
        )

        when:
        UpdateStatistic testObj = emptyStream.collect(UpdateStatistic.collector())

        then:
        with(testObj) {
            getTotal() == 6
            getUpdated() == 0
            getCreated() == 2
            getInvalid() == 1
            getNotChanged() == 3
        }
    }
}
