package com.jvm_bloggers.frontend.admin_area.login.attack

import com.jvm_bloggers.TestTimeProvider
import com.jvm_bloggers.utils.NowProvider
import spock.lang.Specification

import java.time.LocalDateTime
import java.time.Month

/**
 * @author Adam Dec
 */
class BruteForceAttackMailGeneratorSpec extends Specification {

    private static final LocalDateTime DATE = LocalDateTime.of(2016, Month.MARCH, 11, 12, 0, 0)
    private NowProvider nowProvider
    private BruteForceAttackMailGenerator bruteForceAttackMailGenerator

    def setup() {
        nowProvider = new TestTimeProvider(DATE)
        bruteForceAttackMailGenerator = new BruteForceAttackMailGenerator(nowProvider)
    }

    def "Should not prepare mail content due to null BruteForceAttackEvent"() {
        given:
        BruteForceAttackEvent event = null

        when:
        bruteForceAttackMailGenerator.prepareMailContent(event)

        then:
        NullPointerException ex = thrown()
        ex.message == "bruteForceAttackEvent is marked non-null but is null"
    }

    def "Should prepare mail content with null IP address"() {
        given:
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress(null).build()

        when:
        String content = bruteForceAttackMailGenerator.prepareMailContent(event)

        then:
        content == "Brute Force attack detected from IP:  at 12:00:00"
    }

    def "Should prepare mail content with blank IP address"() {
        given:
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress("").build()

        when:
        String content = bruteForceAttackMailGenerator.prepareMailContent(event)

        then:
        content == "Brute Force attack detected from IP:  at 12:00:00"
    }

    def "Should prepare mail content"() {
        given:
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress("127.0.0.1").build()

        when:
        String content = bruteForceAttackMailGenerator.prepareMailContent(event)

        then:
        content == "Brute Force attack detected from IP: 127.0.0.1 at 12:00:00"
    }

    def "Should not prepare mail title due to null BruteForceAttackEvent"() {
        given:
        BruteForceAttackEvent event = null

        when:
        bruteForceAttackMailGenerator.prepareMailTitle(event)

        then:
        NullPointerException ex = thrown()
        ex.message == "bruteForceAttackEvent is marked non-null but is null"
    }

    def "Should prepare mail title with null IP address"() {
        given:
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress(null).build()

        when:
        String content = bruteForceAttackMailGenerator.prepareMailTitle(event)

        then:
        content == "Brute force attack detected for "
    }

    def "Should prepare mail title with blank IP address"() {
        given:
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress("").build()

        when:
        String content = bruteForceAttackMailGenerator.prepareMailTitle(event)

        then:
        content == "Brute force attack detected for "
    }

    def "Should prepare mail title"() {
        given:
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress("127.0.0.1").build()

        when:
        String content = bruteForceAttackMailGenerator.prepareMailTitle(event)

        then:
        content == "Brute force attack detected for 127.0.0.1"
    }
}
