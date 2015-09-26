package pl.tomaszdziurko.jvm_bloggers.people;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.people.domain.Person;
import pl.tomaszdziurko.jvm_bloggers.people.domain.PersonRepository;
import pl.tomaszdziurko.jvm_bloggers.people.json_data.BloggerEntry;
import pl.tomaszdziurko.jvm_bloggers.people.json_data.BloggersData;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
@Slf4j
public class BloggersDataUpdater {

    private final PersonRepository personRepository;
    private final NowProvider nowProvider;

    @Autowired
    public BloggersDataUpdater(PersonRepository personRepository, NowProvider nowProvider) {
        this.personRepository = personRepository;
        this.nowProvider = nowProvider;
    }

    public void updateData(BloggersData data) {
        List<BloggerEntry> entries = data.getBloggers().stream().filter(entry -> entry.getRss().length() > 0).collect(Collectors.toList());
        UpdateSummary updateSummary = new UpdateSummary(entries.size());
        entries.stream().forEach(entry -> updateSingleEntry(entry, updateSummary));
        log.info("Bloggers Data updated: totalRecordsInFile={}, updatedRecords={}, newRecords={}", updateSummary.numberOfEntries,
            updateSummary.updatedEntries, updateSummary.createdEntries);
    }

    protected void updateSingleEntry(BloggerEntry bloggerEntry, UpdateSummary updateSummary) {
        Optional<Person> existingBloggerByJsonId = personRepository.findByJsonId(bloggerEntry.getJsonId());
        Optional<Person> existingBloggerByName = personRepository.findByNameIgnoreCase(bloggerEntry.getName());

        if (existingBloggerByJsonId.isPresent()) {
            Person bloggerWithSameJsonId = existingBloggerByJsonId.get();
            updateBloggerIfThereAreSomeChanges(bloggerEntry, updateSummary, bloggerWithSameJsonId);
        } else if (existingBloggerByName.isPresent()) {
            Person bloggerWithSameName = existingBloggerByName.get();
            updateBloggerIfThereAreSomeChanges(bloggerEntry, updateSummary, bloggerWithSameName);
        } else {
            Person newPerson = new Person(bloggerEntry.getJsonId(), bloggerEntry.getName(),
                StringUtils.lowerCase(bloggerEntry.getRss().toLowerCase()), bloggerEntry.getTwitter(), nowProvider.now());
            personRepository.save(newPerson);
            updateSummary.recordCreated();
        }
    }

    private void updateBloggerIfThereAreSomeChanges(BloggerEntry bloggerEntry, UpdateSummary updateSummary, Person existingBlogger) {
        if (!isEqual(existingBlogger, bloggerEntry)) {
            existingBlogger.setJsonId(bloggerEntry.getJsonId());
            existingBlogger.setName(bloggerEntry.getName());
            existingBlogger.setTwitter(bloggerEntry.getTwitter());
            existingBlogger.setRss(bloggerEntry.getRss());
            personRepository.save(existingBlogger);
            updateSummary.recordUpdated();
        }
    }

    protected boolean isEqual(Person person, BloggerEntry bloggerEntry) {
        return Objects.equals(person.getName(), bloggerEntry.getName())
            && Objects.equals(person.getJsonId(), bloggerEntry.getJsonId())
            && StringUtils.equalsIgnoreCase(person.getRss(), bloggerEntry.getRss())
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
