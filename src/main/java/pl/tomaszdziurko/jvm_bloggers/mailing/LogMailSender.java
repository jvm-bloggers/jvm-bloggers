package pl.tomaszdziurko.jvm_bloggers.mailing;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.util.concurrent.CountDownLatch;

@Component
@Profile({"dev", "test"})
@Slf4j
public class LogMailSender implements MailSender {

    private final CountDownLatch countDownLatch;

    public LogMailSender() {
        this.countDownLatch = new CountDownLatch(1);
    }

    public LogMailSender(CountDownLatch countDownLatch) {
        this.countDownLatch = countDownLatch;
    }

    @Override
    public void sendEmail(String recipientAddress, String subject, String htmlContent) {
        log.debug("Sending email to '{}'\nSubject: {}\n{}", recipientAddress, subject, htmlContent);
        countDownLatch.countDown();
    }

    public CountDownLatch getCountDownLatch() {
        return countDownLatch;
    }
}