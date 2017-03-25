package com.jvm_bloggers.core.data_fetching.blogs

import spock.lang.Specification
import spock.lang.Subject

import java.util.stream.Stream

class UpdateStatisticSpec extends Specification {

    def "Should default to zero"() {
        given:
        Stream<UpdateStatus> emptyStream = Stream.of()

        when:
        @Subject
        UpdateStatistic testObj = emptyStream.collect(UpdateStatistic.collector())

        then:
        testObj.getTotal() == 0
        testObj.getUpdated() == 0
        testObj.getCreated() == 0
        testObj.getInvalid() == 0
        testObj.getNotChanged() == 0
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
        @Subject
        UpdateStatistic testObj = emptyStream.collect(UpdateStatistic.collector())

        then:
        testObj.getTotal() == 6
        testObj.getUpdated() == 0
        testObj.getCreated() == 2
        testObj.getInvalid() == 1
        testObj.getNotChanged() == 3
    }
}
