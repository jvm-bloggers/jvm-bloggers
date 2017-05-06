package com.jvm_bloggers.core.rss.fetchers;

import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.SyndFeedInput;

import javaslang.control.Option;

import lombok.extern.slf4j.Slf4j;

import org.springframework.stereotype.Component;

import java.io.File;

@Component
@Slf4j
public class WgetFetcher implements Fetcher {

    @Override
    public Option<SyndFeed> fetch(String rssUrl) {
        File tempFile = null;
        try {
            tempFile = File.createTempFile("jvm-bloggers", null);
            String tmpPath = tempFile.getAbsolutePath();
            Process process = new ProcessBuilder("wget", rssUrl, "-O", tmpPath).start();
            process.waitFor();
            return Option.of(new SyndFeedInput().build(tempFile));
        } catch (Exception exc) {
            log.warn("Exception during wget execution for url {}: {}", rssUrl, exc.getMessage());
            return Option.none();
        } finally {
            if (tempFile != null) {
                tempFile.delete();
            }
        }
    }
}
