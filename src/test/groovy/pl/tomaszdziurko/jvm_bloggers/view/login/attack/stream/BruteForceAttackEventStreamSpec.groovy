package pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream

import org.apache.commons.lang3.tuple.Pair
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.BruteForceAttackEvent
import rx.Observable
import rx.Observer
import rx.observers.TestSubscriber
import rx.subjects.Subject
import spock.lang.Specification

import java.lang.Void as Should

/**
 * @author Adam Dec
 * @since 0.7.0
 */
class BruteForceAttackEventStreamSpec extends Specification {

    Subject<BruteForceAttackEvent, BruteForceAttackEvent> subject;
    Observable<Pair<String, String>> observable;
    BruteForceAttackEventStream stream;

    def setup() {
        subject = Mock(Subject)
        observable = Observable.empty()
        observable.doOnSubscribe() >> {}
        stream = new BruteForceAttackEventStream(subject, observable)
    }

    Should "Not publish BruteForceAttackEvent to observers"() {
        given:
        subject.hasObservers() >> false
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress("127.0.0.1").build();

        when:
        stream.publish(event)

        then:
        0 * subject.onNext(_)
    }

    Should "Publish BruteForceAttackEvent to observers"() {
        given:
        subject.hasObservers() >> true
        BruteForceAttackEvent event = new BruteForceAttackEvent.BruteForceAttackEventBuilder().ipAddress("127.0.0.1").build();

        when:
        stream.publish(event)

        then:
        1 * subject.onNext(_)
    }

    Should "Subscribe observer to observable"() {
        given:
        TestSubscriber observer = new TestSubscriber()

        when:
        stream.subscribe(observer)

        then:
        observer.assertCompleted()
    }

    Should "Terminate stream"() {
        when:
        stream.terminate()

        then:
        1 * subject.onCompleted()
    }
}