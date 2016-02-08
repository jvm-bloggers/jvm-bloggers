package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.mailing.BlogSummaryMailGenerator;
import pl.tomaszdziurko.jvm_bloggers.mailing.MailSender;
import pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

@Component
@Slf4j
public class MailingPageRequestHandler {

    private MailSender mailSender;
    private BlogSummaryMailGenerator blogSummaryMailGenerator;
    private NowProvider nowProvider;

    public MailingPageRequestHandler() {
    }

    @Autowired
    public MailingPageRequestHandler(BlogSummaryMailGenerator blogSummaryMailGenerator, MailSender mailSender, NowProvider nowProvider) {
        this.blogSummaryMailGenerator = blogSummaryMailGenerator;
        this.mailSender = mailSender;
        this.nowProvider = nowProvider;
    }

    public void sendTestEmail() {
        log.info("Sending test email");
        int daysSinceLastFriday = DateTimeUtilities.daysBetweenDateAndLastFriday(nowProvider.now());
        String mailContent = blogSummaryMailGenerator.prepareMailContent(daysSinceLastFriday);
        mailSender.sendEmail("example@dummy.com", "[JVM Bloggers] Test mail", mailContent);
    }
}
