package pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.Pair;
import pl.tomaszdziurko.jvm_bloggers.mailing.MailSender;
import rx.Subscriber;

/**
 * @author Adam Dec
 * @since 0.7.0
 */
@Slf4j
public class BruteForceLoginAttackMailSubscriber extends Subscriber<Pair<String, String>> {

   private static final String ADMIN_EMAIL_ADDRESS = "admin@admin.pl";

   private final MailSender mailSender;

   public BruteForceLoginAttackMailSubscriber(MailSender mailSender) {
      this.mailSender = mailSender;
   }

   @Override
   public void onStart() {
      request(1);
   }

   @Override
   public void onNext(Pair<String, String> pair) {
      log.debug("Sending e-mail to {} with data={}", ADMIN_EMAIL_ADDRESS, pair);
      mailSender.sendEmail(ADMIN_EMAIL_ADDRESS, pair.getLeft(), pair.getRight());
      request(1);
   }

   @Override
   public void onCompleted() {
      this.unsubscribe();
   }

   @Override
   public void onError(Throwable error) {
      log.error("Could not send mail due to error {}", error);
   }
}