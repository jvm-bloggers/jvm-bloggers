package com.jvm_bloggers.core.rss.fetchers;

import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.SyndFeedInput;

import io.vavr.control.Try;

import lombok.extern.slf4j.Slf4j;

import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.IOException;

import static java.lang.Thread.currentThread;

/**
 * Execute wget unix command, save as rss a file in tmp directory
 * and create SyndFeed from it
 */
@Slf4j
@Component
@Order(Ordered.LOWEST_PRECEDENCE - 1)
public class WgetRssFetcher implements RssFetcher {

    @Override
    public Try<SyndFeed> fetch(String rssUrl) {
        File tempFile = null;
        try {
            tempFile = loadRssToFile(rssUrl);
            return Try.success(new SyndFeedInput().build(tempFile));
        } catch (Exception ex) {
            log.info("Problem during wget execution for url {}: {}", rssUrl, ex.getMessage());
            return Try.failure(ex);
        } finally {
            if (tempFile != null) {
                tempFile.delete();
            }
        }
    }

    protected File loadRssToFile(String rssUrl) throws IOException, InterruptedException {
        File tempFile = File.createTempFile("jvm-bloggers-" + currentThread().getName(), null);
        String tmpPath = tempFile.getAbsolutePath();
        Process process = new ProcessBuilder("wget", rssUrl, "-O", tmpPath).start();
        process.waitFor();
        return tempFile;
    }

}
