package com.jvm_bloggers.core.rss.fetchers;

import com.google.common.collect.ImmutableMap;
import com.jvm_bloggers.core.data_fetching.http.GzipStreamWrapper;
import com.jvm_bloggers.core.data_fetching.http.ProtocolSwitchingAwareConnectionRedirectHandler;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.SyndFeedInput;
import com.rometools.rome.io.XmlReader;

import javaslang.control.Option;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.io.IOUtils;
import org.springframework.stereotype.Component;

import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.Map;

@Slf4j
@Component
@RequiredArgsConstructor
public class HttpFetcher implements Fetcher {

    private static final String FAKE_USER_AGENT = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) "
        + "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.1 Safari/537.36";

    private final ProtocolSwitchingAwareConnectionRedirectHandler redirectHandler;
    private final GzipStreamWrapper gzipStreamWrapper;

    @Override
    public Option<SyndFeed> fetch(String rssUrl) {
        URLConnection urlConnection = null;
        InputStream inputStream = null;
        try {
            urlConnection = new URL(rssUrl).openConnection();
            final Map<String, String> headers =
                ImmutableMap.of("User-Agent", FAKE_USER_AGENT);
            urlConnection = redirectHandler.handle(urlConnection, headers);

            inputStream = gzipStreamWrapper.wrap(urlConnection.getInputStream());
            return Option.of(new SyndFeedInput().build(new XmlReader(inputStream)));
        } catch (Exception ex) {
            log.warn("Error during fetching RSS {} url: {}", rssUrl, ex.getMessage());
            return Option.none();
        } finally {
            IOUtils.closeQuietly(inputStream);
            IOUtils.close(urlConnection);
        }
    }
}
