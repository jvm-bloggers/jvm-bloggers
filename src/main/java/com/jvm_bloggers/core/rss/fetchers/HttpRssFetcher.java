package com.jvm_bloggers.core.rss.fetchers;

import com.google.common.collect.ImmutableMap;
import com.jvm_bloggers.core.data_fetching.http.GzipStreamWrapper;
import com.jvm_bloggers.core.data_fetching.http.ProtocolSwitchingAwareConnectionRedirectHandler;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.SyndFeedInput;
import com.rometools.rome.io.XmlReader;

import io.vavr.control.Try;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.Map;

import static org.apache.commons.io.IOUtils.close;
import static org.apache.commons.io.IOUtils.closeQuietly;

/**
 * Standard https connection
 */
@Slf4j
@Component
@RequiredArgsConstructor
@Order(Ordered.HIGHEST_PRECEDENCE)
public class HttpRssFetcher implements RssFetcher {

    private final ProtocolSwitchingAwareConnectionRedirectHandler redirectHandler;
    private final GzipStreamWrapper gzipStreamWrapper;

    @Override
    public Try<SyndFeed> fetch(String rssUrl) {
        URLConnection urlConnection = null;
        InputStream inputStream = null;
        try {
            urlConnection = new URL(rssUrl).openConnection();
            final Map<String, String> headers =
                ImmutableMap.of("User-Agent", FAKE_USER_AGENT);
            urlConnection = redirectHandler.handle(urlConnection, headers);

            inputStream = gzipStreamWrapper.wrap(urlConnection.getInputStream());
            return Try.success(new SyndFeedInput().build(new XmlReader(inputStream)));
        } catch (Exception ex) {
            log.info("Problem during fetching RSS {} url: {}", rssUrl, ex.getMessage());
            return Try.failure(ex);
        } finally {
            closeQuietly(inputStream);
            close(urlConnection);
        }
    }
}
