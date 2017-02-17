package com.jvm_bloggers.core.data_fetching.blogs;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class BloggersDataFetchingScheduler {

    private final BloggersDataFetcher bloggersDataFetcher;

    @Scheduled(cron = "${scheduler.fetch-bloggers-data}")
    public void fetchBloggersData() {
        log.info("Starting scheduler: bloggers data refresh. This may take a while.");
        bloggersDataFetcher.refreshData();
    }
}
