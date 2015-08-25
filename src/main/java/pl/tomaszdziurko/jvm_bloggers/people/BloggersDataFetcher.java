package pl.tomaszdziurko.jvm_bloggers.people;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.people.json_data.BloggersData;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Optional;

@Component
@Slf4j
public class BloggersDataFetcher {

    private final Optional<URL> urlOptional;
    private final BloggersDataUpdater bloggersDataUpdater;

    @Autowired
    public BloggersDataFetcher(@Value("${bloggers.data.file.url}") String bloggersDataUrlString,
                               BloggersDataUpdater bloggersDataUpdater) {
        urlOptional = convertToUrl(bloggersDataUrlString);
        this.bloggersDataUpdater = bloggersDataUpdater;
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
            bloggersDataUpdater.updateData(bloggers);
        } catch (IOException e) {
            log.error("Exception during parse process", e);
        }
    }

}
