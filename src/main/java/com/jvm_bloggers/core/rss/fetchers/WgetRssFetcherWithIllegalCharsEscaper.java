package com.jvm_bloggers.core.rss.fetchers;

import io.vavr.collection.List;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;

/**
 * Removes characters illegal in XML format to make RSS eader parse feed correctly
 */
@Slf4j
@Component
@Order(Ordered.LOWEST_PRECEDENCE)
public class WgetRssFetcherWithIllegalCharsEscaper extends WgetRssFetcher {

    @Override
    protected File loadRssToFile(String rssUrl) throws IOException, InterruptedException {
        File rssFile = super.loadRssToFile(rssUrl);
        Files.write(
            rssFile.toPath(),
            List.ofAll(Files.readAllLines(rssFile.toPath())).map(existingLine -> {
                String escapedLine = existingLine.replace("\u001B", "");
                return escapedLine;
            }).toJavaList(), StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING
        );
        return rssFile;
    }
}
