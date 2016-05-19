package pl.tomaszdziurko.jvm_bloggers.mailing.sender;

public interface MailSender {

    EmailSendingStatus sendEmail(String fromAddress, String toAddress, String subject,
                                 String htmlContent);

    enum EmailSendingStatus {
        SUCCESS,
        ERROR;

        public boolean isOk() {
            return this.equals(SUCCESS);
        }
    }
}
