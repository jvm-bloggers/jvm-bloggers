package pl.tomaszdziurko.jvm_bloggers.view.login.attack

import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider
import spock.lang.Specification

import java.lang.Void as Should
import java.time.LocalDateTime
import java.time.Month

/**
 * @author Adam Dec
 * @since 0.7.0
 */
class BruteForceAttackMailGeneratorSpec extends Specification {

    NowProvider nowProvider;
    BruteForceAttackMailGenerator bruteForceAttackMailGenerator;

    def setup() {
        nowProvider = Mock(NowProvider)
        nowProvider.now() >> LocalDateTime.of(2016, Month.MARCH, 11, 12, 0, 0)
        bruteForceAttackMailGenerator = new BruteForceAttackMailGenerator(nowProvider)
    }

    Should "Not prepare mail content due to null BruteForceAttackEvent"() {
        given:
        BruteForceAttackEvent event = null;

        when:
        bruteForceAttackMailGenerator.prepareMailContent(event)

        then:
        NullPointerException ex = thrown()
        ex.message.equals("BruteForceAttackEvent can not be null")
    }

    Should "Prepare mail content with null IP address"() {
        given:
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress(null).build();

        when:
        String content = bruteForceAttackMailGenerator.prepareMailContent(event)

        then:
        content.equals("Brute Force attack detected from IP:  at 12:00:00")
    }

    Should "Prepare mail content with blank IP address"() {
        given:
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress("").build();

        when:
        String content = bruteForceAttackMailGenerator.prepareMailContent(event)

        then:
        content.equals("Brute Force attack detected from IP:  at 12:00:00")
    }

    Should "Prepare mail content"() {
        given:
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress("127.0.0.1").build();

        when:
        String content = bruteForceAttackMailGenerator.prepareMailContent(event)

        then:
        content.equals("Brute Force attack detected from IP: 127.0.0.1 at 12:00:00")
    }

    Should "Not prepare mail title due to null BruteForceAttackEvent"() {
        given:
        BruteForceAttackEvent event = null

        when:
        bruteForceAttackMailGenerator.prepareMailTitle(event)

        then:
        NullPointerException ex = thrown()
        ex.message.equals("BruteForceAttackEvent can not be null")
    }

    Should "Prepare mail title with null IP address"() {
        given:
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress(null).build();

        when:
        String content = bruteForceAttackMailGenerator.prepareMailTitle(event)

        then:
        content.equals("Brute force attack detected for ")
    }

    Should "Prepare mail title with blank IP address"() {
        given:
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress("").build();

        when:
        String content = bruteForceAttackMailGenerator.prepareMailTitle(event)

        then:
        content.equals("Brute force attack detected for ")
    }

    Should "Prepare mail title"() {
        given:
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress("127.0.0.1").build();

        when:
        String content = bruteForceAttackMailGenerator.prepareMailTitle(event)

        then:
        content.equals("Brute force attack detected for 127.0.0.1")
    }
}