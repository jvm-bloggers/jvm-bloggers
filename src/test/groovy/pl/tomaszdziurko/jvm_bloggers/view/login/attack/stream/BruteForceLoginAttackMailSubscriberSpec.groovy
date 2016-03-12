package pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream

import org.apache.commons.lang3.tuple.ImmutablePair
import org.apache.commons.lang3.tuple.Pair
import pl.tomaszdziurko.jvm_bloggers.mailing.MailSender
import rx.Observable
import spock.lang.Specification
import java.lang.Void as Should

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

    Should "Request 1 element from emitter on start"() {
        given:
        Observable<Pair<String, String>> observable = Observable.from(new ImmutablePair<String, String>("A", "B"))

        when:
        observable.subscribe(subscriber)

        then:
        1 * mailSender.sendEmail(_, _, _)
    }

    Should "Get 2nd element from emitter on next"() {
        given:
        Observable<Pair<String, String>> observable = Observable.from([new ImmutablePair<String, String>("A", "B"), new ImmutablePair<String, String>("C", "D")])

        when:
        observable.subscribe(subscriber)

        then:
        2 * mailSender.sendEmail(_, _, _)
    }

    Should "Log when exception is thrown"() {
        given:
        Observable<Pair<String, String>> observable = Observable.just(new ImmutablePair<String, String>("A", "B")).map({
            throw new RuntimeException();
        });

        when:
        observable.subscribe(subscriber)

        then:
        0 * mailSender.sendEmail(_, _, _)
    }

    Should "Unsubscribe when Observable calls onCompleted"() {
        given:
        Observable<Pair<String, String>> observable = Observable.just(new ImmutablePair<String, String>("A", "B"));

        when:
        observable.subscribe(subscriber)

        then:
        subscriber.isUnsubscribed()
    }
}
