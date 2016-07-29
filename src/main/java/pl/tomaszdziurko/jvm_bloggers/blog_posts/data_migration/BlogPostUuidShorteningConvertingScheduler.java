package pl.tomaszdziurko.jvm_bloggers.blog_posts.data_migration;


import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.TimeConstants;

@Component
@Slf4j
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class BlogPostUuidShorteningConvertingScheduler {

    private static final int BATCH_SIZE = 250;

    private final BlogPostUidShortener uuidShortener;

    @Scheduled(fixedDelay = TimeConstants.ONE_MINUTE)
    public void convertUidToShorterForm() {
        int numberOfShortenedUids = uuidShortener.shortenUUids(BATCH_SIZE);
        if (numberOfShortenedUids > 0) {
            log.info("Shortened {} urls", numberOfShortenedUids);
        }
    }

}
