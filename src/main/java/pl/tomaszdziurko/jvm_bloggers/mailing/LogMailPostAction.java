package pl.tomaszdziurko.jvm_bloggers.mailing;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.util.concurrent.CountDownLatch;

@Component
@Profile({"dev", "test"})
public class LogMailPostAction implements MailPostAction {

   private CountDownLatch countDownLatch;

   public void init(int actionCount) {
      this.countDownLatch = new CountDownLatch(actionCount);
   }

   @Override
   public void postAction() {
      countDownLatch.countDown();
   }

   @Override
   public void awaitAction() {
      try {
         countDownLatch.await();
      } catch (InterruptedException ignored) {
      }
   }
}