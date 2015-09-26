package pl.tomaszdziurko.jvm_bloggers.people

import pl.tomaszdziurko.jvm_bloggers.people.domain.Person
import pl.tomaszdziurko.jvm_bloggers.people.domain.PersonRepository
import pl.tomaszdziurko.jvm_bloggers.people.json_data.BloggerEntry
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalDateTime

class BloggersDataUpdaterSpec extends Specification {

    PersonRepository personRepository = Mock(PersonRepository)

    @Subject
    BloggersDataUpdater bloggersDataUpdater = new BloggersDataUpdater(personRepository, new NowProvider())

    @Unroll
    def "Should check equality of Person and BloggerEntry"() {
        when:
            boolean isEqual = bloggersDataUpdater.isEqual(person, bloggerEntry)
        then:
            isEqual == expectedResult
        where:
            person                                                    | bloggerEntry                               | expectedResult
            new Person(1L, "name", "rss", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, "name", "rss", "twitter") | true
            new Person(1L, "name", "rss", "twitter", LocalDateTime.now()) | new BloggerEntry(2L, "name", "rss", "twitter") | false
            new Person(1L, "Xame", "rss", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, "name", "rss", "twitter") | false
            new Person(1L, "name", "Xss", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, "name", "rss", "twitter") | false
            new Person(1L, "name", "rss", "Xwitter", LocalDateTime.now()) | new BloggerEntry(1L, "name", "rss", "twitter") | false
            new Person(1L, "namX", "rsX", "twitteX", LocalDateTime.now()) | new BloggerEntry(1L, "name", "rss", "twitter") | false
            new Person(1L, "name", "rss", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, "name", "rss", null)      | false
            new Person(1L, "name", "rss", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, null, "rss", "twitter")   | false
            new Person(1L, "name", "rss", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, "name", null, "twitter")  | false
            new Person(1L, "name", "RSS", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, "name", "rss", "twitter") | true
    }

    def "Should insert new Person for entry with new json_id"() {
        given:
            Long jsonId = 2207L
            BloggerEntry entry = new BloggerEntry(jsonId, "name", "rss", "twitter")
            personRepository.findByJsonId(jsonId) >> Optional.empty()
            personRepository.findByNameIgnoreCase(entry.name) >> Optional.empty()
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            1 * personRepository.save(_ as Person)
            summary.createdEntries == 1
            summary.updatedEntries == 0
    }

    def "Should not update data if equal record already exists in DB"() {
        given:
            Long jsonId = 2207L
            BloggerEntry entry = new BloggerEntry(jsonId, "name", "rss", "twitter")
            personRepository.findByJsonId(jsonId) >> Optional.of(new Person(entry.jsonId, entry.name, entry.rss, entry.twitter, LocalDateTime.now()))
            personRepository.findByNameIgnoreCase(entry.name) >> Optional.empty()
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            0 * personRepository.save(_ as Person)
            summary.createdEntries == 0
            summary.updatedEntries == 0
    }

    def "Should update data if data in entry data differs a bit from record in DB"() {
        given:
            Long jsonId = 2207L
            String rss = "newRssAddress"
            BloggerEntry entry = new BloggerEntry(jsonId, "name", rss, "twitter")
            personRepository.findByJsonId(jsonId) >> Optional.of(new Person(entry.jsonId, entry.name, "oldRSS", entry.twitter, LocalDateTime.now()))
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            1 * personRepository.save(_ as Person)
            summary.createdEntries == 0
            summary.updatedEntries == 1
    }

    def "Should update existing person if only name was changed"() {
        given:
            Long jsonId = 2207L
            String newName = "newName"
            Person existingPerson = new Person(jsonId, "oldName", "existingRSS", "twitter", LocalDateTime.now())
            BloggerEntry entry = new BloggerEntry(existingPerson.jsonId, newName, existingPerson.rss, existingPerson.twitter)
            personRepository.findByJsonId(entry.jsonId) >> Optional.of(existingPerson)
            personRepository.findByNameIgnoreCase(entry.name) >> Optional.empty()
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            1 * personRepository.save({it.jsonId == jsonId && it.name == newName && it.rss == existingPerson.rss &&
                    it.twitter == existingPerson.twitter} )
            summary.createdEntries == 0
            summary.updatedEntries == 1
    }

}
