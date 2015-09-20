package pl.tomaszdziurko.jvm_bloggers.people;


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

    @Scheduled(fixedRate = TimeConstants.THIRTY_MINUTES)
    public void fetchBloggersData() {
        log.info("Starting scheduler: bloggers data refresh");
        bloggersDataFetcher.refreshData();
    }
}
