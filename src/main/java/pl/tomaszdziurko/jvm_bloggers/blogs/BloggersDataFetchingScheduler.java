package pl.tomaszdziurko.jvm_bloggers.blogs;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.TimeConstants;

@Component
@Slf4j
public class BloggersDataFetchingScheduler {

    private final BloggersDataFetcher bloggersDataFetcher;

    @Autowired
    public BloggersDataFetchingScheduler(BloggersDataFetcher bloggersDataFetcher) {
        this.bloggersDataFetcher = bloggersDataFetcher;
    }

    @Scheduled(cron = TimeConstants.EVERY_EIGHT_AM_AND_PM)
    public void fetchBloggersData() {
        log.info("Starting scheduler: bloggers data refresh. This may take a while.");
        bloggersDataFetcher.refreshData();
    }
}
