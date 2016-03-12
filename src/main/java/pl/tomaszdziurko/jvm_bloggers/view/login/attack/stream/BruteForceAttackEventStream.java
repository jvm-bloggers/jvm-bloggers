package pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream;

import org.apache.commons.lang3.tuple.Pair;
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.BruteForceAttackEvent;
import rx.Observable;
import rx.Observer;
import rx.Subscription;
import rx.subjects.Subject;

/**
 * @author Adam Dec
 * @since 0.7.0
 */
public class BruteForceAttackEventStream {

   private final Subject<BruteForceAttackEvent, BruteForceAttackEvent> subject;
   private final Observable<Pair<String, String>> observable;

   public BruteForceAttackEventStream(Subject<BruteForceAttackEvent, BruteForceAttackEvent> subject,
                                      Observable<Pair<String, String>> observable) {
      this.subject = subject;
      this.observable = observable;
   }

   public void publish(BruteForceAttackEvent event) {
      if (subject.hasObservers()) {
         subject.onNext(event);
      }
   }

   public Subscription subscribe(Observer<Pair<String, String>> observer) {
      return observable.subscribe(observer);
   }

   public void terminate() {
      subject.onCompleted();
   }

   public boolean isTerminated() {
       return !subject.hasObservers();
   }
}