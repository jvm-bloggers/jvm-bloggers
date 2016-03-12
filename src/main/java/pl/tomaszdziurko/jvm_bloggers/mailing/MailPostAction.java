package pl.tomaszdziurko.jvm_bloggers.mailing;

/**
 * This interface is intended to use in tests only and probably should be moved from here. <br/>
 *
 * @see pl.tomaszdziurko.jvm_bloggers.mailing.LogMailSender
 * @author Adam Dec
 * @since 0.7.0
 */
public interface MailPostAction {
   void postAction();
   void awaitAction();
}