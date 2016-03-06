package pl.tomaszdziurko.jvm_bloggers.mailing;

public interface MailSender {
    String FROM_NAME = "JVM Bloggers";

    void sendEmail(String recipientAddress, String subject, String htmlContent);
}
