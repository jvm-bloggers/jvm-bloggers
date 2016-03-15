package pl.tomaszdziurko.jvm_bloggers.mailing;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.util.concurrent.CountDownLatch;

/**
 * The main purpose of this class is to act as synchronisation barrier between
 * multiple threads. <br/>
 * Example usage for that is when emails was sent and we want to know how many.
 *
 * @author Adam Dec
 * @see pl.tomaszdziurko.jvm_bloggers.mailing.LogMailSender
 */
@Component
@Profile({"dev", "test"})
public class LogMailSenderPostAction {

    private CountDownLatch countDownLatch;

    public void actionsToWaitOn(int actionCount) {
        this.countDownLatch = new CountDownLatch(actionCount);
    }

    public void postAction() {
        countDownLatch.countDown();
    }

    public void awaitActions() {
        try {
            countDownLatch.await();
        } catch (InterruptedException ignored) {
            // do nothing
        }
    }
}
