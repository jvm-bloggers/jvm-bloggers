package pl.tomaszdziurko.jvm_bloggers.mailing;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class BlogsSummaryMailSendingScheduler {

    public static final int DAYS_IN_THE_PAST_TO_INCLUDE_IN_MAILING = 7;
    private final BlogSummaryMailSender blogSummaryMailSender;

    @Autowired
    public BlogsSummaryMailSendingScheduler(BlogSummaryMailSender blogSummaryMailSender) {
        this.blogSummaryMailSender = blogSummaryMailSender;
    }

    @Scheduled(cron = "0 30 14 * * FRI")
    public void sendBlogsSummaryEmails() {
        log.info("Starting scheduler: sending blogs summary");
        blogSummaryMailSender.sendSummary(DAYS_IN_THE_PAST_TO_INCLUDE_IN_MAILING);
    }
}
