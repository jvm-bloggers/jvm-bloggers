package pl.tomaszdziurko.jvm_bloggers.people;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class BloggersDataFetcher {

    private final Optional<URL> urlOptional;

    @Autowired
    public BloggersDataFetcher(@Value("${bloggers.data.file.url}") String bloggersDataUrlString) {
        urlOptional = convertToUrl(bloggersDataUrlString);
    }

    private Optional<URL> convertToUrl(String urlString)  {
        try {
            return Optional.of(new URL(urlString));
        } catch (MalformedURLException e) {
            log.error("Invalid URL " + urlString);
            return Optional.empty();
        }
    }

    public void refreshData() {
        if (!urlOptional.isPresent()) {
            log.warn("No valid URL specified. Skipping.");
        }

        try {
            ObjectMapper mapper = new ObjectMapper();
            BloggersData bloggers = mapper.readValue(urlOptional.get(), BloggersData.class);
            log.info("Size = " + bloggers.bloggers.size());
        } catch (IOException e) {
            log.error("Exception during parse process", e);
        }
    }

    @Data
    private static class BloggerEntry {
        private String name;
        private String rss;
        private String twitter;
    }

    @Data
    private static class BloggersData {
        private List<BloggerEntry> bloggers;

        public BloggersData(List<BloggerEntry> bloggers) {
            this.bloggers = bloggers;
        }

        public BloggersData() {
        }
    }
}
