package pl.tomaszdziurko.jvm_bloggers.utils;

import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.FeedException;
import com.rometools.rome.io.SyndFeedInput;
import com.rometools.rome.io.XmlReader;

import lombok.extern.slf4j.Slf4j;

import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.http.RedirectableConnection;

import java.io.IOException;
import java.io.InputStream;
import java.util.Optional;

@Component
@Slf4j
public class SyndFeedProducer {

    public Optional<SyndFeed> createFor(String rssUrl) {
        try (RedirectableConnection connection = new RedirectableConnection(rssUrl);
             InputStream inputStream = connection.getInputStream();
             XmlReader reader = new XmlReader(inputStream)) {
            SyndFeed syndFeed = new SyndFeedInput().build(reader);
            return Optional.of(syndFeed);
        } catch (IOException | FeedException ex) {
            log.warn("Error during fetching RSS {} url", rssUrl, ex);
            return Optional.empty();
        }
    }

    public Optional<String> urlFromRss(String rss) {
        return createFor(rss).map(SyndFeed::getLink);
    }
}
