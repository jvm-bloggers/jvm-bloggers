package pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream;

import org.apache.commons.lang3.tuple.Pair;
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.BruteForceAttackEvent;
import rx.Observable;
import rx.Observer;
import rx.Subscription;
import rx.subjects.PublishSubject;

public class BruteForceAttackEventStream {

   private final PublishSubject<BruteForceAttackEvent> inputSubject;
   private final Observable<Pair<String, String>> observable;

   public BruteForceAttackEventStream(PublishSubject<BruteForceAttackEvent> inputSubject,
                                      Observable<Pair<String, String>> observable) {
      this.inputSubject = inputSubject;
      this.observable = observable;
   }

   public void publish(BruteForceAttackEvent event) {
      if (inputSubject.hasObservers()) {
         inputSubject.onNext(event);
      }
   }

   public Subscription subscribe(Observer<Pair<String, String>> observer) {
      return observable.subscribe(observer);
   }

   public void terminate() {
      inputSubject.onCompleted();
   }

   public static class Builder {

      private PublishSubject<BruteForceAttackEvent> inputSubject;
      private Observable<Pair<String, String>> observable;

      public Builder withInputSubject(PublishSubject<BruteForceAttackEvent> inputSubject) {
         this.inputSubject = inputSubject;
         return this;
      }

      public Builder withObservable(Observable<Pair<String, String>> observable) {
         this.observable = observable;
         return this;
      }

      public BruteForceAttackEventStream build() {
         return new BruteForceAttackEventStream(inputSubject, observable);
      }
   }
}