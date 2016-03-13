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

    private static final String ADMIN_EMAIL_ADDRESS = "admin@admin.pl";
    private static final int NUMBER_OF_ITEMS_TO_REQUEST = 1;

    private final MailSender mailSender;

    public BruteForceLoginAttackMailSubscriber(MailSender mailSender) {
        this.mailSender = mailSender;
    }

    @Override
    public void onStart() {
        request(NUMBER_OF_ITEMS_TO_REQUEST);
    }

    @Override
    public void onNext(final Pair<String, String> pair) {
        log.debug("Sending e-mail to [{}] with data={}", ADMIN_EMAIL_ADDRESS, pair);
        mailSender.sendEmail(ADMIN_EMAIL_ADDRESS, pair.getLeft(), pair.getRight());
        request(NUMBER_OF_ITEMS_TO_REQUEST);
    }

    @Override
    public void onCompleted() {
        unsubscribe();
    }

    @Override
    public void onError(final Throwable error) {
        log.error("Could not send e-mail to [{}] due to error", ADMIN_EMAIL_ADDRESS, error);
    }
}