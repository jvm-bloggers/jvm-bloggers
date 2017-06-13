package com.jvm_bloggers.core.data_fetching.blogs;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class BloggersDataFetchingScheduler {

    private final BloggersDataFetcher bloggersDataFetcher;

    @Scheduled(cron = "${scheduler.fetch-bloggers-data}")
    public void fetchBloggersData() {
        log.info("Starting scheduler: bloggers data refresh. This may take a while.");
        bloggersDataFetcher.refreshData();
    }
}
