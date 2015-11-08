package pl.tomaszdziurko.jvm_bloggers.utils;

import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.io.FeedException;
import com.sun.syndication.io.SyndFeedInput;
import com.sun.syndication.io.XmlReader;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

@Slf4j
@Component
public class SyndFeedProducer {

    public static final String FAKE_USER_AGENT =
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.1 Safari/537.36";

    public SyndFeed createFor(String rssUrl) {
        try {
            SyndFeedInput syndFeedInput = new SyndFeedInput();
            URLConnection urlConnection = new URL(rssUrl).openConnection();
            urlConnection.addRequestProperty("User-Agent", FAKE_USER_AGENT);
            try (InputStream inputStream = urlConnection.getInputStream()) {
                return syndFeedInput.build(new XmlReader(inputStream));
            }
        } catch (IOException | FeedException e) {
            throw new RuntimeException(e);
        }

    }
}
