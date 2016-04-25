package pl.tomaszdziurko.jvm_bloggers.blogs

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType;
import pl.tomaszdziurko.jvm_bloggers.blogs.json_data.BloggerEntry
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider
import pl.tomaszdziurko.jvm_bloggers.utils.SyndFeedProducer
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalDateTime

import static pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType.COMPANY
import static pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType.PERSONAL

class BloggersDataUpdaterSpec extends Specification {

    BlogRepository blogRepository = Mock(BlogRepository)

    @Subject
    BloggersDataUpdater bloggersDataUpdater = new BloggersDataUpdater(blogRepository, new NowProvider(), spySyndFeedProducer())

    @Unroll
    def "Should check equality of Person and BloggerEntry"() {
        when:
            boolean isEqual = bloggersDataUpdater.isEqual(person, bloggerEntry)
        then:
            isEqual == expectedResult
        where:
            person                                    | bloggerEntry                                              | expectedResult
            buildBlog(1L, "blog", "rss", "twitter")   | buildBloggerEntry(1L, "blog", "rss", "twitter", PERSONAL) | true
            buildBlog(1L, "blog", "rss", "twitter")   | buildBloggerEntry(1L, "blog", "rss", "twitter", COMPANY)  | false
            buildBlog(1L, "blog", "rss", "twitter")   | buildBloggerEntry(2L, "blog", "rss", "twitter", PERSONAL) | false
            buildBlog(1L, "Author", "rss", "twitter") | buildBloggerEntry(1L, "blog", "rss", "twitter", PERSONAL) | false
            buildBlog(1L, "blog", "Xss", "twitter")   | buildBloggerEntry(1L, "blog", "rss", "twitter", PERSONAL) | false
            buildBlog(1L, "blog", "rss", "Xwitter")   | buildBloggerEntry(1L, "blog", "rss", "twitter", PERSONAL) | false
            buildBlog(1L, "authoX", "rsX", "twitteX") | buildBloggerEntry(1L, "blog", "rss", "twitter", PERSONAL) | false
            buildBlog(1L, "blog", "rss", "twitter")   | buildBloggerEntry(1L, "blog", "rss", null, PERSONAL)      | false
            buildBlog(1L, "blog", "rss", "twitter")   | buildBloggerEntry(1L, null, "rss", "twitter", PERSONAL)   | false
            buildBlog(1L, "blog", "rss", "twitter")   | buildBloggerEntry(1L, "blog", null, "twitter", PERSONAL)  | false
            buildBlog(1L, "blog", "RSS", "twitter")   | buildBloggerEntry(1L, "blog", "rss", "twitter", PERSONAL) | true
    }

    def "Should insert new Person for entry with new json_id"() {
        given:
            Long jsonId = 2207L
            BloggerEntry entry = buildBloggerEntry(jsonId, "blog", "rss", "twitter", null)
            blogRepository.findByJsonId(jsonId) >> Optional.empty()
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            1 * blogRepository.save(_ as Blog)
            summary.createdEntries == 1
            summary.updatedEntries == 0
    }

    def "Should not update data if equal record already exists in DB"() {
        given:
            Long jsonId = 2207L
            BloggerEntry entry = buildBloggerEntry(jsonId, "blog", "rss", "twitter", PERSONAL)
            blogRepository.findByJsonId(jsonId) >> Optional.of(buildBlog(entry.jsonId, entry.name, entry.rss, entry.twitter))
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            0 * blogRepository.save(_ as Blog)
            summary.createdEntries == 0
            summary.updatedEntries == 0
    }

    def "Should update data if data in entry data differs a bit from record in DB"() {
        given:
            Long jsonId = 2207L
            String rss = "newRssAddress"
            BloggerEntry entry = new BloggerEntry(jsonId, "blog", rss, "twitter", PERSONAL)
            blogRepository.findByJsonId(jsonId) >> Optional.of(buildBlog(entry.jsonId, entry.name, "oldRSS", entry.twitter))
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            1 * blogRepository.save(_ as Blog)
            summary.createdEntries == 0
            summary.updatedEntries == 1
    }

    def "Should update existing person if only name was changed"() {
        given:
            Long jsonId = 2207L
            String newName = "newAuthor"
            Blog existingPerson = buildBlog(jsonId, "oldAuthor", "existingRSS", "twitter")
            BloggerEntry entry = new BloggerEntry(existingPerson.jsonId, newName, existingPerson.rss, existingPerson.twitter, COMPANY)
            blogRepository.findByJsonId(entry.jsonId) >> Optional.of(existingPerson)
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            1 * blogRepository.save({
                it.jsonId == jsonId && it.author == newName && it.rss == existingPerson.rss &&
                        it.twitter == existingPerson.twitter
            })
            summary.createdEntries == 0
            summary.updatedEntries == 1
    }
    
    def "Should update existing blog if url was changed"() {
        given:
            Long jsonId = 2207L
            Blog blog = buildBlog(
                jsonId, "author", "http://blog.pl/rss", "http://old.blog.pl",
                "twitter", LocalDateTime.now(), PERSONAL
            )
            BloggerEntry entry = new BloggerEntry(
                blog.jsonId, blog.author, blog.rss, blog.twitter, COMPANY
            )
            blogRepository.findByJsonId(entry.jsonId) >> Optional.of(blog)
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            1 * blogRepository.save({
                it.url = "http://new.blog.pl"
            })
    }
    
    def "Should not update existing blog url if new address could not be retrieved"() {
        given:
            Long jsonId = 2207L
            Blog blog = buildBlog(
                jsonId, "author", "http://blog.pl/rss", "http://old.blog.pl",
                "twitter", LocalDateTime.now(), PERSONAL
            )
            BloggerEntry entry = new BloggerEntry(
                blog.jsonId, blog.author, blog.rss, blog.twitter, COMPANY
            )
            blogRepository.findByJsonId(entry.jsonId) >> Optional.of(blog)
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            0 * blogRepository.save({
                it.url = ""
            })
    }

    def buildBlog(Long jsonId, String author, String rss, String twitter) {
        buildBlog(jsonId, author, rss, null, twitter, LocalDateTime.now(), PERSONAL)
    }

    def buildBlog(Long jsonId, String author, String rss, String url, String twitter, LocalDateTime dateAdded, BlogType type) {
        return Blog.builder()
            .jsonId(jsonId)
            .author(author)
            .rss(rss)
            .url(url)
            .twitter(twitter)
            .dateAdded(LocalDateTime.now())
            .blogType(type)
            .build()
    }
    
    def buildBloggerEntry(Long jsonId, String author, String rss, String twitter, BlogType type) {
        return new BloggerEntry(jsonId, author, rss, twitter, type)
    }
    
    def spySyndFeedProducer() {
        SyndFeedProducer producer = Spy(SyndFeedProducer);
        producer.validUrlFromRss("") >> Optional.empty()
        producer.validUrlFromRss("http://blog.pl/rss") >> Optional.of("http://new.blog.pl/")
        return producer
    }
}
