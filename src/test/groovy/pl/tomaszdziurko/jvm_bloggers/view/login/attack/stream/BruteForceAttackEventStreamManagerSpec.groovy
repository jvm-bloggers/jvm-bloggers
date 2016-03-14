package pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream

import org.springframework.test.util.ReflectionTestUtils
import pl.tomaszdziurko.jvm_bloggers.mailing.LogMailSender
import pl.tomaszdziurko.jvm_bloggers.mailing.LogMailSenderPostAction
import pl.tomaszdziurko.jvm_bloggers.settings.Setting
import pl.tomaszdziurko.jvm_bloggers.settings.SettingKeys
import pl.tomaszdziurko.jvm_bloggers.settings.SettingRepository
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.BruteForceAttackEvent
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.BruteForceAttackMailGenerator
import rx.Scheduler
import rx.schedulers.TestScheduler
import spock.lang.Specification

import java.util.concurrent.TimeUnit

import static BruteForceAttackEventStreamManager.MAILING_TIME_THROTTLE_IN_MINUTES

/**
 * @author Adam Dec
 */
class BruteForceAttackEventStreamManagerSpec extends Specification {

    LogMailSender mailSender;
    LogMailSenderPostAction logMailPostAction;
    Scheduler scheduler;
    SettingRepository settingRepository;
    BruteForceAttackEventStreamManager manager;

    def setup() {
        logMailPostAction = new LogMailSenderPostAction();
        mailSender = new LogMailSender(logMailPostAction)
        settingRepository = Mock(SettingRepository);
        BruteForceAttackMailGenerator bruteForceAttackMailGenerator = Mock(BruteForceAttackMailGenerator)
        bruteForceAttackMailGenerator.prepareMailContent(_) >> "A"
        bruteForceAttackMailGenerator.prepareMailTitle(_) >> "B"
        scheduler = new TestScheduler()

        // Close your eyes! :D
        manager = new BruteForceAttackEventStreamManager()
        ReflectionTestUtils.setField(manager, "mailSender", mailSender)
        ReflectionTestUtils.setField(manager, "bruteForceAttackMailGenerator", bruteForceAttackMailGenerator)
        ReflectionTestUtils.setField(manager, "scheduler", scheduler)
        ReflectionTestUtils.setField(manager, "settingRepository", settingRepository)
    }

    def "Should throw Runtime exception when ADMIN_EMAIL setting is not found"() {
        given:
            settingRepository.findByName(_) >> null
        when:
            manager.init()
        then:
            RuntimeException ex = thrown()
            ex.message.equals(SettingKeys.ADMIN_EMAIL.toString() + " not found in Setting table")
    }

    def "Should return admin email setting"() {
        given:
            settingRepository.findByName(_) >> new Setting(SettingKeys.ADMIN_EMAIL.toString(), "admin@jvmbloggers.pl")
        when:
            manager.init()
        then:
            ReflectionTestUtils.getField(manager, "adminEmailAddress") == "admin@jvmbloggers.pl"
    }

    def "Should build BruteForceAttackEventStream for given clientAddress only once"() {
        given:
            String clientAddress = "127.0.0.1"
        when:
            BruteForceAttackEventStream eventStream = manager.createEventStreamFor(clientAddress)
        then:
            eventStream == manager.createEventStreamFor(clientAddress)
    }

    def "Should send an email"() {
        given:
            logMailPostAction.actionsToWaitOn(1)
            String clientAddress = "127.0.0.1"
            BruteForceAttackEventStream eventStream = manager.createEventStreamFor(clientAddress)
        when:
            eventStream.publish(BruteForceAttackEvent.builder().ipAddress(clientAddress).build());
            scheduler.advanceTimeTo(MAILING_TIME_THROTTLE_IN_MINUTES, TimeUnit.MINUTES);
        then:
            mailSender.getLogMailSenderPostAction().awaitActions();
    }

    def "Should send an email only three times"() {
        given:
            logMailPostAction.actionsToWaitOn(3)
            String clientAddress = "127.0.0.1"
            BruteForceAttackEventStream eventStream = manager.createEventStreamFor(clientAddress)
            int counter = 1
        when:
            (1..19).each {
                eventStream.publish(BruteForceAttackEvent.builder().ipAddress(clientAddress).build());
                if (it.intValue() % 5 == 0) {
                    scheduler.advanceTimeTo(MAILING_TIME_THROTTLE_IN_MINUTES * (counter++), TimeUnit.MINUTES);
                }
            }
        then:
            mailSender.getLogMailSenderPostAction().awaitActions()
    }

    def "Should terminate all streams on destroy"() {
        given:
            String clientAddress = "127.0.0.1"
            BruteForceAttackEventStream eventStream = manager.createEventStreamFor(clientAddress)
        when:
            manager.destroy();
            scheduler.advanceTimeTo(MAILING_TIME_THROTTLE_IN_MINUTES, TimeUnit.MINUTES);
        then:
            eventStream.isTerminated()
    }
}
