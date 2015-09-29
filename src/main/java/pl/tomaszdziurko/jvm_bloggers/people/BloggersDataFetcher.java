package pl.tomaszdziurko.jvm_bloggers.people;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.people.json_data.BloggersData;

import java.io.IOException;
import java.util.Optional;

@Component
@Slf4j
public class BloggersDataFetcher {

    private final Optional<Resource> resourceOptional;
    private final BloggersDataUpdater bloggersDataUpdater;

    @Autowired
    public BloggersDataFetcher(@Value("${bloggers.data.file.url}") Resource resource,
                               BloggersDataUpdater bloggersDataUpdater) {
        log.info("Bloggers resource = '{}' ", resource.getDescription());
        resourceOptional = convertToFile(resource);
        this.bloggersDataUpdater = bloggersDataUpdater;
    }

    private Optional<Resource> convertToFile(Resource resource)  {
        if (!resource.exists()) {
            log.error("Invalid file path {}", resource);
            return Optional.empty();
        } else {
            return Optional.of(resource);
        }
    }

    public void refreshData() {

        if (!resourceOptional.isPresent()) {
            log.warn("No valid File path specified. Skipping.");
        }

        try {
            ObjectMapper mapper = new ObjectMapper();
            BloggersData bloggers = mapper.readValue(resourceOptional.get().getURL(), BloggersData.class);
            log.debug("Bloggers from file:  {}", bloggers);
            bloggersDataUpdater.updateData(bloggers);
        } catch (IOException e) {
            log.error("Exception during parse process", e);
        }
    }
}
