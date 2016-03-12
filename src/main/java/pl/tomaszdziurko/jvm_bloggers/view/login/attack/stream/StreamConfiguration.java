package pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import rx.Scheduler;
import rx.schedulers.Schedulers;
import rx.schedulers.TestScheduler;

/**
 * @author Adam Dec
 * @since 0.7.0
 */
@Configuration
public class StreamConfiguration {

   @Bean
   @Profile({"!test"})
   public Scheduler scheduler() {
      return Schedulers.computation();
   }

   @Bean
   @Profile({"test"})
   public Scheduler testScheduler() {
      return new TestScheduler();
   }
}