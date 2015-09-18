package pl.tomaszdziurko.jvm_bloggers.mailing;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.TimeConstants;

@Component
@Slf4j
public class BlogsSummaryMailSendingScheduler {

    public static final int TWO_WEEKS_BACK_IN_THE_PAST = 14;
    private BlogSummaryMailSender blogSummaryMailSender;

    @Autowired
    public BlogsSummaryMailSendingScheduler(BlogSummaryMailSender blogSummaryMailSender) {
        this.blogSummaryMailSender = blogSummaryMailSender;
    }

    @Scheduled(fixedRate = TimeConstants.THIRTY_MINUTES)
    public void sendBlogsSummaryEmails() {
        log.info("Starting scheduler: sending blogs summary");
        blogSummaryMailSender.sendSummary(TWO_WEEKS_BACK_IN_THE_PAST);
    }
}
