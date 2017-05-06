package com.jvm_bloggers.core.rss.fetchers;

import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.SyndFeedInput;

import javaslang.control.Option;

import lombok.extern.slf4j.Slf4j;

import org.springframework.stereotype.Component;

import java.io.File;
import java.util.UUID;

@Component
@Slf4j
public class WgetFetcher implements Fetcher {

    @Override
    public Option<SyndFeed> fetch(String rssUrl) {
        String uid = UUID.randomUUID().toString();
        try {
            Process process = new ProcessBuilder("wget", rssUrl, "-O", "/tmp/" + uid).start();
            process.waitFor();
            File file = new File("/tmp/" + uid);
            Option<SyndFeed> feed = Option.of(new SyndFeedInput().build(file));
            file.delete();
            return feed;
        } catch (Exception exc) {
            log.warn("Exception during wget execution for url {}: {}", rssUrl, exc.getMessage());
            return Option.none();
        }
    }
}
