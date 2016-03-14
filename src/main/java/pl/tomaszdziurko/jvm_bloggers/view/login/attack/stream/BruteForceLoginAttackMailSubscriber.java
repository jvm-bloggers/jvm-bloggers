package pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream;

import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.tuple.Pair;

import pl.tomaszdziurko.jvm_bloggers.mailing.MailSender;

import rx.Subscriber;

/**
 * @author Adam Dec
 */
@Slf4j
public class BruteForceLoginAttackMailSubscriber extends Subscriber<Pair<String, String>> {

    private static final int NUMBER_OF_ITEMS_TO_REQUEST = 1;

    private final MailSender mailSender;
    private final String adminEmailAddress;

    public BruteForceLoginAttackMailSubscriber(MailSender mailSender, String adminEmailAddress) {
        this.mailSender = mailSender;
        this.adminEmailAddress = adminEmailAddress;
    }

    @Override
    public void onStart() {
        request(NUMBER_OF_ITEMS_TO_REQUEST);
    }

    @Override
    public void onNext(final Pair<String, String> pair) {
        log.debug("Sending e-mail to [{}] with data={}", adminEmailAddress, pair);
        mailSender.sendEmail(adminEmailAddress, pair.getLeft(), pair.getRight());
        request(NUMBER_OF_ITEMS_TO_REQUEST);
    }

    @Override
    public void onCompleted() {
        unsubscribe();
    }

    @Override
    public void onError(final Throwable error) {
        log.error("Could not send e-mail to [{}] due to error", adminEmailAddress, error);
    }
}
