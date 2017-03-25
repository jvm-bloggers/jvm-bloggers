package com.jvm_bloggers.frontend.admin_area.login.attack

import spock.lang.Specification

import java.util.concurrent.TimeUnit

class BruteForceLoginAttackDetectorSpec extends Specification {

    BruteForceLoginAttackDetector detector = new BruteForceLoginAttackDetector(1, TimeUnit.SECONDS)

    def "Should detect brute force attack after three invalid login attempts"() {
        given:
        String ipAddress = "testIpAddress"

        when:
        (1..3).each {
            detector.recordInvalidLoginAttempt(ipAddress)
        }

        then:
        detector.isItBruteForceAttack(ipAddress)
    }

    def "Should not detect brute force attack after two invalid login attempts"() {
        given:
        String ipAddress = "testIpAddress"

        when:
        (1..2).each {
            detector.recordInvalidLoginAttempt(ipAddress)
        }

        then:
        !detector.isItBruteForceAttack(ipAddress)
    }

    def "Should not detect brute force attack after invalid login attempts from different IPs"() {
        given:
        String ipAddress = "testIpAddress"

        when:
        (1..10).each {
            detector.recordInvalidLoginAttempt(ipAddress + it)
        }

        then:
        (1..10).each {
            !detector.isItBruteForceAttack(ipAddress + it)
        }
    }

    def "Should let invalid login attempts expire"() {
        given:
        String ipAddress = "testIpAddress"

        when:
        (1..3).each {
            detector.recordInvalidLoginAttempt(ipAddress)
        }
        sleep(1001)
        detector.recordInvalidLoginAttempt(ipAddress)

        then:
        !detector.isItBruteForceAttack(ipAddress)
    }
}