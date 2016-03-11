package pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream;

import com.google.common.annotations.VisibleForTesting;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.mailing.MailSender;
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.BruteForceAttackEvent;
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.BruteForceAttackMailGenerator;
import rx.Observable;
import rx.Scheduler;
import rx.schedulers.Schedulers;
import rx.subjects.PublishSubject;

import javax.annotation.PreDestroy;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;

@Slf4j
@Component
public class BruteForceAttackEventStreamFactory {

   @VisibleForTesting
   public static final int MAILING_TIME_THROTTLE_IN_MINUTES = 1;
   private static final int INITIAL_CAPACITY = 1024;


   private final ConcurrentMap<String, BruteForceAttackEventStream> observableMap = new ConcurrentHashMap<>(INITIAL_CAPACITY);
   private final MailSender mailSender;
   private final BruteForceAttackMailGenerator bruteForceAttackMailGenerator;
   private final Scheduler scheduler;

   @Autowired
   public BruteForceAttackEventStreamFactory(MailSender mailSender, BruteForceAttackMailGenerator bruteForceAttackMailGenerator, Scheduler scheduler) {
      this.mailSender = mailSender;
      this.bruteForceAttackMailGenerator = bruteForceAttackMailGenerator;
      this.scheduler = scheduler;
   }

   public BruteForceAttackEventStream build(String clientAddress) {
      return observableMap.computeIfAbsent(clientAddress, key -> {
         final PublishSubject<BruteForceAttackEvent> subject = PublishSubject.create();
         final Observable<Pair<String, String>> observable = subject.sample(MAILING_TIME_THROTTLE_IN_MINUTES, TimeUnit.MINUTES, scheduler)
            .map(this::buildTuple)
            .observeOn(Schedulers.io())
            .onBackpressureLatest();
         final BruteForceAttackEventStream stream = new BruteForceAttackEventStream.Builder().withInputSubject(subject).withObservable(observable).build();
         stream.subscribe(new BruteForceLoginAttackMailSubscriber(mailSender));
         return stream;
      });
   }

   @PreDestroy
   public void destroy() {
      observableMap.forEach((k, v) -> {
         log.debug("Terminating stream for clientAddress={}", k);
         v.terminate();
      });
   }

   private final Pair<String, String> buildTuple(BruteForceAttackEvent event) {
      final String mailContent = bruteForceAttackMailGenerator.prepareMailContent(event);
      log.info("Mail content\n{}", mailContent);

      final String mailTitle = bruteForceAttackMailGenerator.prepareEmailTitle(event);
      log.info("Mail title\n{}", mailTitle);

      return new ImmutablePair<String, String>(mailTitle, mailContent);
   }
}