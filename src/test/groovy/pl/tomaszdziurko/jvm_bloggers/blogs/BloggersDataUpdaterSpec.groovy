package pl.tomaszdziurko.jvm_bloggers.blogs

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository
import pl.tomaszdziurko.jvm_bloggers.blogs.json_data.BloggerEntry
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalDateTime

class BloggersDataUpdaterSpec extends Specification {

    BlogRepository personRepository = Mock(BlogRepository)

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
            new Blog(1L, "blog", "rss", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, "blog", "rss", "twitter") | true
            new Blog(1L, "blog", "rss", "twitter", LocalDateTime.now()) | new BloggerEntry(2L, "blog", "rss", "twitter") | false
            new Blog(1L, "Author", "rss", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, "blog", "rss", "twitter") | false
            new Blog(1L, "blog", "Xss", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, "blog", "rss", "twitter") | false
            new Blog(1L, "blog", "rss", "Xwitter", LocalDateTime.now()) | new BloggerEntry(1L, "blog", "rss", "twitter") | false
            new Blog(1L, "authoX", "rsX", "twitteX", LocalDateTime.now()) | new BloggerEntry(1L, "blog", "rss", "twitter") | false
            new Blog(1L, "blog", "rss", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, "blog", "rss", null)      | false
            new Blog(1L, "blog", "rss", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, null, "rss", "twitter")   | false
            new Blog(1L, "blog", "rss", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, "blog", null, "twitter")  | false
            new Blog(1L, "blog", "RSS", "twitter", LocalDateTime.now()) | new BloggerEntry(1L, "blog", "rss", "twitter") | true
    }

    def "Should insert new Person for entry with new json_id"() {
        given:
            Long jsonId = 2207L
            BloggerEntry entry = new BloggerEntry(jsonId, "blog", "rss", "twitter")
            personRepository.findByJsonId(jsonId) >> Optional.empty()
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            1 * personRepository.save(_ as Blog)
            summary.createdEntries == 1
            summary.updatedEntries == 0
    }

    def "Should not update data if equal record already exists in DB"() {
        given:
            Long jsonId = 2207L
            BloggerEntry entry = new BloggerEntry(jsonId, "blog", "rss", "twitter")
            personRepository.findByJsonId(jsonId) >> Optional.of(new Blog(entry.jsonId, entry.name, entry.rss, entry.twitter, LocalDateTime.now()))
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            0 * personRepository.save(_ as Blog)
            summary.createdEntries == 0
            summary.updatedEntries == 0
    }

    def "Should update data if data in entry data differs a bit from record in DB"() {
        given:
            Long jsonId = 2207L
            String rss = "newRssAddress"
            BloggerEntry entry = new BloggerEntry(jsonId, "blog", rss, "twitter")
            personRepository.findByJsonId(jsonId) >> Optional.of(new Blog(entry.jsonId, entry.name, "oldRSS", entry.twitter, LocalDateTime.now()))
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            1 * personRepository.save(_ as Blog)
            summary.createdEntries == 0
            summary.updatedEntries == 1
    }

    def "Should update existing person if only name was changed"() {
        given:
            Long jsonId = 2207L
            String newName = "newAuthor"
            Blog existingPerson = new Blog(jsonId, "oldAuthor", "existingRSS", "twitter", LocalDateTime.now())
            BloggerEntry entry = new BloggerEntry(existingPerson.jsonId, newName, existingPerson.rss, existingPerson.twitter)
            personRepository.findByJsonId(entry.jsonId) >> Optional.of(existingPerson)
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            1 * personRepository.save({it.jsonId == jsonId && it.author == newName && it.rss == existingPerson.rss &&
                    it.twitter == existingPerson.twitter} )
            summary.createdEntries == 0
            summary.updatedEntries == 1
    }

}
