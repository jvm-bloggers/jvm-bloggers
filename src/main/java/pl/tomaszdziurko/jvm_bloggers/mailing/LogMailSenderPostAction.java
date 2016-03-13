package pl.tomaszdziurko.jvm_bloggers.mailing;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.util.concurrent.CountDownLatch;

/**
 * This class is intended to use in tests only and probably should be moved from here. <br/>
 * The main purpose of this class is to act as synchronisation barrier between multiple threads. <br/>
 * The very perfect usage for that is when emails was sent and we want to know how many.
 *
 * @author Adam Dec
 * @see pl.tomaszdziurko.jvm_bloggers.mailing.LogMailSender
 */
@Component
@Profile({"dev", "test"})
public class LogMailSenderPostAction {

    private CountDownLatch countDownLatch;

    public void init(int actionCount) {
        this.countDownLatch = new CountDownLatch(actionCount);
    }

    public void postAction() {
        countDownLatch.countDown();
    }

    public void awaitAction() {
        try {
            countDownLatch.await();
        } catch (InterruptedException ignored) {
        }
    }
}