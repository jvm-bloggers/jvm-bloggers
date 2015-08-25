package pl.tomaszdziurko.jvm_bloggers.people;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.people.json_data.BloggerEntry;
import pl.tomaszdziurko.jvm_bloggers.people.json_data.BloggersData;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
@Slf4j
public class BloggersDataUpdater {

    private PersonRepository personRepository;

    @Autowired
    public BloggersDataUpdater(PersonRepository personRepository) {
        this.personRepository = personRepository;
    }

    public void updateData(BloggersData data) {
        List<BloggerEntry> entries = data.getBloggers().stream().filter(entry -> entry.getRss().length() > 0).collect(Collectors.toList());
        UpdateSummary updateSummary = new UpdateSummary(entries.size());
        entries.stream().forEach(entry -> updateSingleEntry(entry, updateSummary));
        log.info("Bloggers Data updated: totalRecordsInFile={}, updatedRecords={}, newRecords={}", updateSummary.numberOfEntries,
            updateSummary.updatedEntries, updateSummary.createdEntries);
    }

    protected void updateSingleEntry(BloggerEntry bloggerEntry, UpdateSummary updateSummary) {
        Optional<Person> existingBloggerByRss = personRepository.findByRss(bloggerEntry.getRss());

        if (existingBloggerByRss.isPresent()) {
            if (!isEqual(existingBloggerByRss.get(), bloggerEntry)) {
                Person person = existingBloggerByRss.get();
                person.setName(bloggerEntry.getName());
                person.setTwitter(bloggerEntry.getTwitter());
                personRepository.save(person);
                updateSummary.recordUpdated();
            }
        } else {
            Person newPerson = new Person(bloggerEntry.getName(), bloggerEntry.getRss().toLowerCase(), bloggerEntry.getTwitter());
            personRepository.save(newPerson);
            updateSummary.recordCreated();
        }
    }

    protected boolean isEqual(Person person, BloggerEntry bloggerEntry) {
        return Objects.equals(person.getName(), bloggerEntry.getName())
            && Objects.equals(person.getRss(), bloggerEntry.getRss())
            && Objects.equals(person.getTwitter(), bloggerEntry.getTwitter());
    }

    @Getter
    public static class UpdateSummary {

        private int numberOfEntries;
        private int updatedEntries;
        private int createdEntries;

        public UpdateSummary(int numberOfEntries) {
            this.numberOfEntries = numberOfEntries;
        }

        public void recordUpdated() {
            updatedEntries++;
        }

        public void recordCreated() {
            createdEntries++;
        }
    }

}
