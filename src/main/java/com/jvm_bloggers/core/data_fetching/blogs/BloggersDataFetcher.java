package com.jvm_bloggers.core.data_fetching.blogs;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.routing.RoundRobinPool;

import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogType;
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

@Component
@Slf4j
public class BloggersDataFetcher {

    private final Optional<URL> bloggersUrlOptional;
    private final Optional<URL> companiesUrlOptional;
    private final Optional<URL> videosUrlOptional;
    private final MetadataRepository metadataRepository;
    private final NowProvider nowProvider;
    private final ActorRef bloggerDataFetcherActor;

    @Autowired
    public BloggersDataFetcher(@Value("${bloggers.data.file.url}") String bloggersDataUrlString,
                               @Value("${companies.data.file.url}") String companiesDataUrlString,
                               @Value("${youtube.data.file.url}") String videosDataUrlString,
                               ActorSystem actorSystem,
                               BloggersDataUpdater bloggersDataUpdater,
                               MetadataRepository metadataRepository,
                               NowProvider nowProvider) {
        bloggersUrlOptional = convertToUrl(bloggersDataUrlString);
        companiesUrlOptional = convertToUrl(companiesDataUrlString);
        videosUrlOptional = convertToUrl(videosDataUrlString);
        this.metadataRepository = metadataRepository;
        this.nowProvider = nowProvider;
        bloggerDataFetcherActor =
            actorSystem.actorOf(new RoundRobinPool(3)
                    .props(BloggersDataFetcherActor.props(bloggersDataUpdater)),
                "bloggers-data-fetcher");
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
        bloggerDataFetcherActor
            .tell(new BloggersUrlWithType(bloggersUrlOptional, BlogType.PERSONAL),
                ActorRef.noSender());
        bloggerDataFetcherActor
            .tell(new BloggersUrlWithType(companiesUrlOptional, BlogType.COMPANY),
                ActorRef.noSender());
        bloggerDataFetcherActor
            .tell(new BloggersUrlWithType(videosUrlOptional, BlogType.VIDEOS), ActorRef.noSender());

        final Metadata dateOfLastFetch = metadataRepository
            .findByName(MetadataKeys.DATE_OF_LAST_FETCHING_BLOGGERS);
        dateOfLastFetch.setValue(nowProvider.now().toString());
        metadataRepository.save(dateOfLastFetch);
    }
}
