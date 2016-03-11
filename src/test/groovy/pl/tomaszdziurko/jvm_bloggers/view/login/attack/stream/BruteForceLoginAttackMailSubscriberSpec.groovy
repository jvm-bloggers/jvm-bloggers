package pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream

import pl.tomaszdziurko.jvm_bloggers.mailing.MailSender
import spock.lang.Specification

/**
 * @author Adam Dec
 * @since 0.7.0
 */
class BruteForceLoginAttackMailSubscriberSpec extends Specification {

    MailSender mailSender;
    BruteForceLoginAttackMailSubscriber subscriber;

    def setup() {
        mailSender = Mock(MailSender)
        subscriber = new BruteForceLoginAttackMailSubscriber(mailSender)
    }
}
