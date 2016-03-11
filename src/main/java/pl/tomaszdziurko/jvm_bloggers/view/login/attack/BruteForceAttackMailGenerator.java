package pl.tomaszdziurko.jvm_bloggers.view.login.attack;

import lombok.extern.slf4j.Slf4j;
import org.antlr.stringtemplate.StringTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

import java.time.format.DateTimeFormatter;

@Component
@Slf4j
public class BruteForceAttackMailGenerator {

   private static final String DEFAULT_EMAIL_TITLE = "Brute force attack detected for $ClientAddress$";
   private static final String ATTACK_BRUTE_FORCE_MAIL_TEMPLATE = "Brute Force attack detected from IP: $ClientAddress$ at $Time$";
   private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("HH:mm:ss");
   private static final String TIME = "Time";
   private static final String CLIENT_ADDRESS = "ClientAddress";

   private final NowProvider nowProvider;

   @Autowired
   public BruteForceAttackMailGenerator(NowProvider nowProvider) {
      this.nowProvider = nowProvider;
   }

   public String prepareMailContent(BruteForceAttackEvent bruteForceAttackEvent) {
      final StringTemplate template = new StringTemplate(ATTACK_BRUTE_FORCE_MAIL_TEMPLATE);
      template.setAttribute(CLIENT_ADDRESS, bruteForceAttackEvent.getClientAddress());
      template.setAttribute(TIME, nowProvider.now().format(FORMATTER));
      return template.toString();
   }

   public String prepareEmailTitle(BruteForceAttackEvent bruteForceAttackEvent) {
      final StringTemplate template = new StringTemplate(DEFAULT_EMAIL_TITLE);
      template.setAttribute(CLIENT_ADDRESS, bruteForceAttackEvent.getClientAddress());
      return template.toString();
   }
}