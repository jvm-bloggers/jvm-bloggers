package pl.tomaszdziurko.jvm_bloggers.view.login.attack;

import com.google.common.base.Preconditions;
import lombok.extern.slf4j.Slf4j;
import org.antlr.stringtemplate.StringTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

import java.time.format.DateTimeFormatter;

/**
 * @author Adam Dec
 * @since 0.7.0
 */
@Component
@Slf4j
public class BruteForceAttackMailGenerator {

   private static final String TIME = "Time";
   private static final String IP_ADDRESS = "IPAddress";
   private static final String DEFAULT_EMAIL_TITLE = "Brute force attack detected for $" + IP_ADDRESS + "$";
   private static final String ATTACK_BRUTE_FORCE_MAIL_TEMPLATE = "Brute Force attack detected from IP: $" + IP_ADDRESS + "$ at $" + TIME + "$";
   private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("HH:mm:ss");

   private final NowProvider nowProvider;

   @Autowired
   public BruteForceAttackMailGenerator(NowProvider nowProvider) {
      this.nowProvider = nowProvider;
   }

   public String prepareMailContent(BruteForceAttackEvent bruteForceAttackEvent) {
      Preconditions.checkNotNull(bruteForceAttackEvent, "BruteForceAttackEvent can not be null");

      final StringTemplate template = new StringTemplate(ATTACK_BRUTE_FORCE_MAIL_TEMPLATE);
      template.setAttribute(IP_ADDRESS, bruteForceAttackEvent.getIpAddress());
      template.setAttribute(TIME, nowProvider.now().format(FORMATTER));
      return template.toString();
   }

   public String prepareMailTitle(BruteForceAttackEvent bruteForceAttackEvent) {
      Preconditions.checkNotNull(bruteForceAttackEvent, "BruteForceAttackEvent can not be null");

      final StringTemplate template = new StringTemplate(DEFAULT_EMAIL_TITLE);
      template.setAttribute(IP_ADDRESS, bruteForceAttackEvent.getIpAddress());
      return template.toString();
   }
}