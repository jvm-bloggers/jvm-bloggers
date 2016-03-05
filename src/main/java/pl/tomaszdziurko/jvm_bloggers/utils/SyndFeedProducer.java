package pl.tomaszdziurko.jvm_bloggers.utils;

import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.springframework.stereotype.Component;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.io.SyndFeedInput;
import com.sun.syndication.io.XmlReader;

import lombok.Cleanup;
import lombok.SneakyThrows;
import pl.tomaszdziurko.jvm_bloggers.http.ProtocolSwitchingAwareConnectionRedirectHandler;

@Component
public class SyndFeedProducer {

    public static final String FAKE_USER_AGENT =
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.1 Safari/537.36";

    private final ProtocolSwitchingAwareConnectionRedirectHandler redirectHandler = new ProtocolSwitchingAwareConnectionRedirectHandler();

    @SneakyThrows
    public SyndFeed createFor(String rssUrl) {
        URLConnection urlConnection = null;
        try {
            urlConnection = new URL(rssUrl).openConnection();
            final Map<String, List<String>> headers = ImmutableMap.of("User-Agent", ImmutableList.of(FAKE_USER_AGENT));
            urlConnection = redirectHandler.handle(urlConnection, headers);
            @Cleanup
            final InputStream inputStream = urlConnection.getInputStream();
            return new SyndFeedInput().build(new XmlReader(inputStream));
        } finally {
            IOUtils.close(urlConnection);
        }
    }
}
