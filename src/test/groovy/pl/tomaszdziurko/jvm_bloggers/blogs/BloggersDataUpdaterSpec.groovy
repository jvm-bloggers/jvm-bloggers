package pl.tomaszdziurko.jvm_bloggers.blogs

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType
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

    public static final String RSS_OF_VALID_BLOG = "http://blog.pl/rss"
    public static final String RSS_OF_INVALID_BLOG = "http://invalidblog.pl/rss"
    BlogRepository blogRepository = Mock(BlogRepository)

    @Subject
    BloggersDataUpdater bloggersDataUpdater = new BloggersDataUpdater(blogRepository, new NowProvider(), spySyndFeedProducer())

    @Unroll
    def "Should detect that BloggerEntry is different than corresponding Blog"() {
        when:
            boolean somethingChanged = bloggersDataUpdater.somethingChangedInBloggerData(person, bloggerEntry)
        then:
            somethingChanged == expectedResult
        where:
            person                                 | bloggerEntry                | expectedResult
            standardPersonalBlog()                 | entryWithStandardBlogData() | false
            blogWithUppercasedRss()                | entryWithStandardBlogData() | false
            standardPersonalBlog()                 | entryWithCompanyBlogData()  | true
            blogWithDifferentJsonId()              | entryWithStandardBlogData() | true
            blogWithDifferentAuthor()              | entryWithStandardBlogData() | true
            blogWithDifferentRss()                 | entryWithStandardBlogData() | true
            blogWithDifferentTwitter()             | entryWithStandardBlogData() | true
            blogWithDifferentAuthorRssAndTwitter() | entryWithStandardBlogData() | true
            standardPersonalBlog()                 | entryWithDifferentPage()    | true
    }

    private BloggerEntry entryWithCompanyBlogData() {
        buildBloggerEntry(1L, "blog", "rss", "page", "twitter", COMPANY)
    }

    private BloggerEntry entryWithDifferentPage() {
        buildBloggerEntry(1L, "blog", "rss", "newPage", "twitter", PERSONAL)
    }

    private Blog standardPersonalBlog() {
        buildBlog(1L, "blog", "rss", "page", "twitter")
    }

    private BloggerEntry entryWithStandardBlogData() {
        buildBloggerEntry(1L, "blog", "rss", "page", "twitter", PERSONAL)
    }

        private Blog blogWithDifferentAuthorRssAndTwitter() {
        buildBlog(1L, "authoX", "rsX", "page", "twitteX")
    }

    private Blog blogWithDifferentTwitter() {
        buildBlog(1L, "blog", "rss", "page", "Xwitter")
    }

    private Blog blogWithDifferentRss() {
        buildBlog(1L, "blog", "Xss", "page", "twitter")
    }

    private Blog blogWithDifferentAuthor() {
        buildBlog(1L, "Author", "rss", "page", "twitter")
    }

    private Blog blogWithDifferentJsonId() {
        buildBlog(2L, "blog", "rss", "page", "twitter")
    }

    private Blog blogWithUppercasedRss() {
        buildBlog(1L, "blog", "RSS", "page", "twitter")
    }

    def "Should insert new Person for entry with new json_id"() {
        given:
            Long jsonId = 2207L
            BloggerEntry entry = buildBloggerEntry(jsonId, "blog", RSS_OF_VALID_BLOG, "page", "twitter", PERSONAL)
            blogRepository.findByJsonId(jsonId) >> Optional.empty()
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            1 * blogRepository.save(_ as Blog)
            summary.createdEntries == 1
            summary.updatedEntries == 0
    }

    def "Should skip insertion of new Person for entry without valid url in rss"() {
        given:
            Long jsonId = 2207L
            BloggerEntry entry = buildBloggerEntry(jsonId, "blog", RSS_OF_INVALID_BLOG, "page", "twitter", PERSONAL)
            blogRepository.findByJsonId(jsonId) >> Optional.empty()
        when:
            BloggersDataUpdater.UpdateSummary summary = new BloggersDataUpdater.UpdateSummary(1)
            bloggersDataUpdater.updateSingleEntry(entry, summary)
        then:
            0 * blogRepository.save(_ as Blog)
            summary.createdEntries == 0
            summary.updatedEntries == 0
    }

    def "Should not update data if equal record already exists in DB"() {
        given:
            Long jsonId = 2207L
            BloggerEntry entry = buildBloggerEntry(jsonId, "blog", "rss", "page", "twitter", PERSONAL)
            blogRepository.findByJsonId(jsonId) >> Optional.of(buildBlog(entry.jsonId, entry.name, entry.rss, entry.url, entry.twitter))
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
            String rss = "httP://newRssAddress"
            BloggerEntry entry = new BloggerEntry(jsonId, "blog", rss, "twitter", PERSONAL)
            blogRepository.findByJsonId(jsonId) >> Optional.of(buildBlog(entry.jsonId, entry.name, "oldRSS", entry.rss, entry.twitter))
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
            Blog existingPerson = buildBlog(jsonId, "oldAuthor", "http://existingRSS", "page", "twitter")
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
            String newBlogUrl = "http://new.blog.pl"
            Long jsonId = 2207L
            Blog blog = buildBlog(
                    jsonId, "author", RSS_OF_VALID_BLOG, "http://old.blog.pl",
                    "twitter", LocalDateTime.now(), PERSONAL
            )
            BloggerEntry entry = new BloggerEntry(
                    blog.jsonId, blog.author, blog.rss, newBlogUrl, blog.twitter, COMPANY
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
                    jsonId, "author", RSS_OF_VALID_BLOG, "http://old.blog.pl",
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

    def buildBlog(Long jsonId, String author, String rss, String pageUrl, String twitter) {
        buildBlog(jsonId, author, rss, pageUrl, twitter, LocalDateTime.now(), PERSONAL)
    }

    def buildBlog(Long jsonId, String author, String rss, String url, String twitter, LocalDateTime dateAdded, BlogType type) {
        return Blog.builder()
                .jsonId(jsonId)
                .author(author)
                .rss(rss)
                .url(url)
                .twitter(twitter)
                .dateAdded(dateAdded)
                .blogType(type)
                .build()
    }

    def buildBloggerEntry(Long jsonId, String author, String rss, String pageUrl, String twitter, BlogType type) {
        return new BloggerEntry(jsonId, author, rss, pageUrl, twitter, type)
    }

    def spySyndFeedProducer() {
        SyndFeedProducer producer = Spy(SyndFeedProducer);
        producer.validUrlFromRss("") >> Optional.empty()
        producer.validUrlFromRss(RSS_OF_VALID_BLOG) >> Optional.of("http://new.blog.pl/")
        return producer
    }
}
