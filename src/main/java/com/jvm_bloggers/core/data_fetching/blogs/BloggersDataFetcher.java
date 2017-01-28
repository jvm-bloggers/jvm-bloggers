package com.jvm_bloggers.core.data_fetching.blogs;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogType;
import com.jvm_bloggers.core.data_fetching.blogs.json_data.BloggersData;
import com.jvm_bloggers.core.metadata.Metadata;
import com.jvm_bloggers.core.metadata.MetadataKeys;
import com.jvm_bloggers.core.metadata.MetadataRepository;
import com.jvm_bloggers.utils.NowProvider;

import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Optional;

@Component
@Slf4j
public class BloggersDataFetcher {

    private final Optional<URL> bloggersUrlOptional;
    private final Optional<URL> companiesUrlOptional;
    private final Optional<URL> videosUrlOptional;
    private final BloggersDataUpdater bloggersDataUpdater;
    private final ObjectMapper mapper;
    private final MetadataRepository metadataRepository;
    private final NowProvider nowProvider;
    private final PreventConcurrentExecutionSafeguard concurrentExecutionSafeguard
        = new PreventConcurrentExecutionSafeguard();

    @Autowired
    public BloggersDataFetcher(@Value("${bloggers.data.file.url}") String bloggersDataUrlString,
                               @Value("${companies.data.file.url}") String companiesDataUrlString,
                               @Value("${youtube.data.file.url}") String videosDataUrlString,
                               BloggersDataUpdater bloggersDataUpdater,
                               ObjectMapper mapper, MetadataRepository metadataRepository,
                               NowProvider nowProvider) {
        bloggersUrlOptional = convertToUrl(bloggersDataUrlString);
        companiesUrlOptional = convertToUrl(companiesDataUrlString);
        videosUrlOptional = convertToUrl(videosDataUrlString);
        this.bloggersDataUpdater = bloggersDataUpdater;
        this.mapper = mapper;
        this.metadataRepository = metadataRepository;
        this.nowProvider = nowProvider;
    }

    private Optional<URL> convertToUrl(String urlString) {
        try {
            return Optional.of(new URL(urlString));
        } catch (MalformedURLException exception) {
            log.error("Invalid URL " + urlString);
            return Optional.empty();
        }
    }

    public void refreshData() {
        concurrentExecutionSafeguard.preventConcurrentExecution(this::startFetchingProcess);
    }

    @Async("singleThreadExecutor")
    public void refreshDataAsynchronously() {
        refreshData();
    }

    private Void startFetchingProcess() {
        refreshBloggersDataFor(bloggersUrlOptional, BlogType.PERSONAL);
        refreshBloggersDataFor(companiesUrlOptional, BlogType.COMPANY);
        refreshBloggersDataFor(videosUrlOptional, BlogType.VIDEOS);

        final Metadata dateOfLastFetch = metadataRepository
            .findByName(MetadataKeys.DATE_OF_LAST_FETCHING_BLOGGERS);
        dateOfLastFetch.setValue(nowProvider.now().toString());
        metadataRepository.save(dateOfLastFetch);
        return null;
    }

    private void refreshBloggersDataFor(Optional<URL> blogsDataUrl, BlogType blogType) {
        if (blogsDataUrl.isPresent()) {
            try {
                BloggersData bloggers = mapper.readValue(blogsDataUrl.get(), BloggersData.class);
                bloggers.getBloggers().forEach(it -> it.setBlogType(blogType));
                UpdateStatistic updateStatistic = bloggersDataUpdater.updateData(bloggers);
                log.info("Refreshed {} blogs: {}", blogType, updateStatistic);
            } catch (Exception exception) {
                log.error("Exception during parse process for {}", blogType, exception);
            }
        } else {
            log.warn("No valid URL specified for {}. Skipping.", blogType);
        }
    }

    public boolean isFetchingProcessInProgress() {
        return concurrentExecutionSafeguard.isExecuting();
    }
}
