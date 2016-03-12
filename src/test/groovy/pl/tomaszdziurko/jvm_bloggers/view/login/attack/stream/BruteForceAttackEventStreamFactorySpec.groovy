package pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.test.util.ReflectionTestUtils
import pl.tomaszdziurko.jvm_bloggers.mailing.LogMailPostAction
import pl.tomaszdziurko.jvm_bloggers.mailing.LogMailSender
import pl.tomaszdziurko.jvm_bloggers.mailing.MailPostAction
import pl.tomaszdziurko.jvm_bloggers.mailing.MailSender
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.BruteForceAttackEvent
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.BruteForceAttackMailGenerator
import rx.Scheduler
import rx.schedulers.TestScheduler
import spock.lang.Specification
import java.lang.Void as Should
import java.util.concurrent.TimeUnit

import static pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream.BruteForceAttackEventStreamFactory.MAILING_TIME_THROTTLE_IN_MINUTES

/**
 * @author Adam Dec
 * @since 0.7.0
 */
class BruteForceAttackEventStreamFactorySpec extends Specification {

    LogMailSender mailSender;
    LogMailPostAction logMailPostAction;
    Scheduler scheduler;
    BruteForceAttackEventStreamFactory factory;

    def setup() {
        logMailPostAction = new LogMailPostAction();
        mailSender = new LogMailSender(logMailPostAction)
        BruteForceAttackMailGenerator bruteForceAttackMailGenerator = Mock(BruteForceAttackMailGenerator)
        bruteForceAttackMailGenerator.prepareMailContent(_) >> "A"
        bruteForceAttackMailGenerator.prepareMailTitle(_) >> "B"
        scheduler = new TestScheduler()

        // Close your eyes! :D
        factory = new BruteForceAttackEventStreamFactory()
        ReflectionTestUtils.setField(factory, "mailSender", mailSender)
        ReflectionTestUtils.setField(factory, "bruteForceAttackMailGenerator", bruteForceAttackMailGenerator)
        ReflectionTestUtils.setField(factory, "scheduler", scheduler)
    }

    Should "Build BruteForceAttackEventStream for given clientAddress only once"() {
        given:
        String clientAddress = "127.0.0.1"

        when:
        BruteForceAttackEventStream eventStream = factory.build(clientAddress)

        then:
        eventStream == factory.build(clientAddress)
    }

    Should "Send an email"() {
        given:
        logMailPostAction.init(1)
        String clientAddress = "127.0.0.1"
        BruteForceAttackEventStream eventStream = factory.build(clientAddress)

        when:
        eventStream.publish(BruteForceAttackEvent.builder().ipAddress(clientAddress).build());
        scheduler.advanceTimeTo(MAILING_TIME_THROTTLE_IN_MINUTES, TimeUnit.MINUTES);

        then:
        mailSender.getMailPostAction().awaitAction();
    }

    Should "Send an email only three times"() {
        given:
        logMailPostAction.init(3)
        String clientAddress = "127.0.0.1"
        BruteForceAttackEventStream eventStream = factory.build(clientAddress)
        int counter = 1

        when:
        (1..19).each {
            eventStream.publish(BruteForceAttackEvent.builder().ipAddress(clientAddress).build());
            if (it.intValue() % 5 == 0) {
                scheduler.advanceTimeTo(MAILING_TIME_THROTTLE_IN_MINUTES * (counter++), TimeUnit.MINUTES);
            }
        }

        then:
        mailSender.getMailPostAction().awaitAction()
    }

    Should "Terminate all streams on destroy"() {
        given:
        String clientAddress = "127.0.0.1"
        BruteForceAttackEventStream eventStream = factory.build(clientAddress)

        when:
        factory.destroy();
        scheduler.advanceTimeTo(MAILING_TIME_THROTTLE_IN_MINUTES, TimeUnit.MINUTES);

        then:
        eventStream.isTerminated()
    }
}
