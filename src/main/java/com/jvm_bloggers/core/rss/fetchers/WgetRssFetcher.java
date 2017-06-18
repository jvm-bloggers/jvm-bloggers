package com.jvm_bloggers.core.rss.fetchers;

import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.SyndFeedInput;

import javaslang.control.Try;

import lombok.extern.slf4j.Slf4j;

import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import java.io.File;

/**
 * Execute wget unix command, save as rss a file in tmp directory
 * and create SyndFeed from it
 */
@Slf4j
@Component
@Order(Ordered.LOWEST_PRECEDENCE)
public class WgetRssFetcher implements RssFetcher {

    @Override
    public Try<SyndFeed> fetch(String rssUrl) {
        File tempFile = null;
        try {
            tempFile = File.createTempFile("jvm-bloggers", null);
            String tmpPath = tempFile.getAbsolutePath();
            Process process = new ProcessBuilder("wget", rssUrl, "-O", tmpPath).start();
            process.waitFor();
            return Try.success(new SyndFeedInput().build(tempFile));
        } catch (Exception ex) {
            log.warn("Exception during wget execution for url {}: {}", rssUrl, ex.getMessage());
            return Try.failure(ex);
        } finally {
            if (tempFile != null) {
                tempFile.delete();
            }
        }
    }
}
