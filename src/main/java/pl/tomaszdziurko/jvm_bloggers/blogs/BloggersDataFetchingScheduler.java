package pl.tomaszdziurko.jvm_bloggers.blogs;


import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.TimeConstants;
import pl.tomaszdziurko.jvm_bloggers.metadata.Metadata;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataKeys;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

@Slf4j
@Component
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class BloggersDataFetchingScheduler {

    private final BloggersDataFetcher bloggersDataFetcher;
    private final MetadataRepository metadataRepository;
    private final NowProvider nowProvider;

    @Scheduled(cron = TimeConstants.EVERY_EIGHT_AM_AND_PM)
    public void fetchBloggersData() {
        log.info("Starting scheduler: bloggers data refresh");
        bloggersDataFetcher.refreshData();
        final Metadata dateOfLastFetch = metadataRepository
                .findByName(MetadataKeys.DATE_OF_LAST_FETCHING_BLOGGERS.toString());
        dateOfLastFetch.setValue(nowProvider.now().toString());
        metadataRepository.save(dateOfLastFetch);
    }
}
