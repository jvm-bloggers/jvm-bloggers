package com.jvm_bloggers.core.data_fetching.blogs;

import akka.actor.AbstractActor;
import akka.actor.Props;
import akka.japi.pf.ReceiveBuilder;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogType;
import com.jvm_bloggers.core.data_fetching.blogs.json_data.BloggersData;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.net.URL;
import java.util.Optional;

@Slf4j
public class BloggersDataFetcherActor extends AbstractActor {

    private final BloggersDataUpdater bloggersDataUpdater;
    private final ObjectMapper mapper = new ObjectMapper();

    public BloggersDataFetcherActor(BloggersDataUpdater bloggersDataUpdater) {
        this.bloggersDataUpdater = bloggersDataUpdater;
        receive(ReceiveBuilder.match(BloggersUrlWithType.class, bloggersUrlWithType ->
            refreshBloggersDataFor(bloggersUrlWithType.getBlogsDataUrl(),
                bloggersUrlWithType.getBlogType())).build());
    }

    private void refreshBloggersDataFor(Optional<URL> blogsDataUrl, BlogType blogType) {
        if (blogsDataUrl.isPresent()) {
            try {
                BloggersData bloggers = mapper.readValue(blogsDataUrl.get(), BloggersData.class);
                bloggers.getBloggers().stream().forEach(it -> it.setBlogType(blogType));
                bloggersDataUpdater.updateData(bloggers);
            } catch (IOException exception) {
                log.error("Exception during parse process for {}", blogType, exception);
            }
        } else {
            log.warn("No valid URL specified for {}. Skipping.", blogType);
        }
    }

    public static Props props(BloggersDataUpdater bloggersDataUpdater) {
        return Props.create(BloggersDataFetcherActor.class,
            () -> new BloggersDataFetcherActor(bloggersDataUpdater));
    }
}
