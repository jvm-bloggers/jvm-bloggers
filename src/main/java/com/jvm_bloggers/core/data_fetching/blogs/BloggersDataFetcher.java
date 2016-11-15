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
import org.springframework.stereotype.Component;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Optional;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

@Component
@Slf4j
public class BloggersDataFetcher {

    private final Optional<URL> bloggersUrlOptional;
    private final Optional<URL> companiesUrlOptional;
    private final Optional<URL> videosUrlOptional;
    private final BloggersDataUpdater bloggersDataUpdater;
    private final ObjectMapper mapper = new ObjectMapper();
    private final MetadataRepository metadataRepository;
    private final NowProvider nowProvider;

    private static final ThreadPoolExecutor
        fetchingBloggersDataExecutor = new ThreadPoolExecutor(1, 1,
        0L, TimeUnit.MILLISECONDS,
        new LinkedBlockingQueue<>());

    @Autowired
    public BloggersDataFetcher(@Value("${bloggers.data.file.url}") String bloggersDataUrlString,
                               @Value("${companies.data.file.url}") String companiesDataUrlString,
                               @Value("${youtube.data.file.url}") String videosDataUrlString,
                               BloggersDataUpdater bloggersDataUpdater,
                               MetadataRepository metadataRepository,
                               NowProvider nowProvider) {
        bloggersUrlOptional = convertToUrl(bloggersDataUrlString);
        companiesUrlOptional = convertToUrl(companiesDataUrlString);
        videosUrlOptional = convertToUrl(videosDataUrlString);
        this.bloggersDataUpdater = bloggersDataUpdater;
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
        if (isFetchingProcessInProgress()) {
            log.info("Fetching bloggers data already in progress");
            return;
        }
        fetchingBloggersDataExecutor
            .submit(this::startFetchingProcess);
    }

    public void startFetchingProcess() {
        refreshBloggersDataFor(bloggersUrlOptional, BlogType.PERSONAL);
        refreshBloggersDataFor(companiesUrlOptional, BlogType.COMPANY);
        refreshBloggersDataFor(videosUrlOptional, BlogType.VIDEOS);

        final Metadata dateOfLastFetch = metadataRepository
            .findByName(MetadataKeys.DATE_OF_LAST_FETCHING_BLOGGERS);
        dateOfLastFetch.setValue(nowProvider.now().toString());
        metadataRepository.save(dateOfLastFetch);
    }

    private void refreshBloggersDataFor(Optional<URL> blogsDataUrl, BlogType blogType) {
        if (blogsDataUrl.isPresent()) {
            try {
                BloggersData bloggers = mapper.readValue(blogsDataUrl.get(), BloggersData.class);
                bloggers.getBloggers().stream().forEach(it -> it.setBlogType(blogType));
                bloggersDataUpdater.updateData(bloggers);
            } catch (Exception exception) {
                log.error("Exception during parse process for {}", blogType, exception);
            }
        } else {
            log.warn("No valid URL specified for {}. Skipping.", blogType);
        }
    }

    public boolean isFetchingProcessInProgress() {
        return fetchingBloggersDataExecutor.getActiveCount() != 0;
    }
}
