package com.jvm_bloggers.core.data_fetching.blogs

import com.jvm_bloggers.core.data_fetching.blogs.json_data.BloggerEntry
import com.jvm_bloggers.core.data_fetching.blogs.json_data.BloggersData
import com.jvm_bloggers.core.rss.SyndFeedProducer
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.utils.ZoneTimeProvider
import io.vavr.control.Option
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime

import static com.jvm_bloggers.entities.blog.BlogType.COMPANY
import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

@Subject(BloggersDataUpdater)
class BloggersDataUpdaterSpec extends Specification {

    public static final String RSS_OF_VALID_BLOG = 'http://blog.pl/rss'
    public static final String RSS_OF_INVALID_BLOG = 'http://invalidblog.pl/rss'

    BlogRepository blogRepository = Mock(BlogRepository)
    SyndFeedProducer producer = syndFeedProducer()
    BloggersDataUpdater bloggersDataUpdater = new BloggersDataUpdater(blogRepository, new ZoneTimeProvider(), producer, new BloggerChangedVerifier())

    def "Should insert new Person for entry with new bookmarkable_id"() {
        given:
        String bookmarkableId = 'bookmarkableId-2207'
        BloggerEntry entry = buildBloggerEntry(bookmarkableId, 'blog', RSS_OF_VALID_BLOG, 'page', 'twitter', PERSONAL)
        blogRepository.findByBookmarkableId(bookmarkableId) >> Option.none()
        BloggersData bloggers = buildBloggersData(entry)

        when:
        UpdateStatistic statistics = bloggersDataUpdater.updateData(bloggers)

        then:
        1 * blogRepository.save(_ as Blog)
        statistics.getCreated() == 1
    }

    def "Should skip insertion of new Person for entry without valid url in rss"() {
        given:
        String bookmarkableId = 'bookmarkableId-2207'
        BloggerEntry entry = buildBloggerEntry(bookmarkableId, 'blog', RSS_OF_INVALID_BLOG, 'page', 'twitter', PERSONAL)
        blogRepository.findByBookmarkableId(bookmarkableId) >> Option.none()
        BloggersData bloggers = buildBloggersData(entry)
        producer.validUrlFromRss(RSS_OF_INVALID_BLOG) >> Option.none()

        when:
        UpdateStatistic statistics = bloggersDataUpdater.updateData(bloggers)

        then:
        0 * blogRepository.save(_ as Blog)
        statistics.getInvalid() == 1
    }

    def "Should not update data if equal record already exists in DB"() {
        given:
        String bookmarkableId = 'bookmarkableId-2207'
        BloggerEntry entry = buildBloggerEntry(bookmarkableId, 'blog', 'rss', 'page', 'twitter', PERSONAL)
        blogRepository.findByBookmarkableId(bookmarkableId) >> Option.of(buildBlog(entry.bookmarkableId, entry.name, entry.rss, entry.url, entry.twitter))
        BloggersData bloggers = buildBloggersData(entry)

        when:
        UpdateStatistic statistics = bloggersDataUpdater.updateData(bloggers)

        then:
        0 * blogRepository.save(_ as Blog)
        statistics.getNotChanged() == 1
    }

    def "Should update data if data in entry data differs a bit from record in DB"() {
        given:
        String bookmarkableId = 'bookmarkableId-2207'
        BloggerEntry entry = new BloggerEntry(bookmarkableId, 'blog', 'http://newRssAddress', 'twitter', PERSONAL)
        blogRepository.findByBookmarkableId(bookmarkableId) >> Option.of(buildBlog(entry.bookmarkableId, entry.name, 'oldRSS', entry.rss, entry.twitter))
        BloggersData bloggers = buildBloggersData(entry)

        when:
        UpdateStatistic statistics = bloggersDataUpdater.updateData(bloggers)

        then:
        1 * blogRepository.save(_ as Blog)
        statistics.getUpdated() == 1
    }

    def "Should update existing person if only name was changed"() {
        given:
        String bookmarkableId = 'bookmarkableId-2207'
        String newName = 'newAuthor'
        Blog existingPerson = buildBlog(bookmarkableId,'oldAuthor', 'http://existingRSS', 'page', 'twitter')
        BloggerEntry entry = new BloggerEntry(existingPerson.bookmarkableId, newName, existingPerson.rss, existingPerson.twitter, COMPANY)
        blogRepository.findByBookmarkableId(entry.bookmarkableId) >> Option.of(existingPerson)
        BloggersData bloggers = buildBloggersData(entry)

        when:
        UpdateStatistic statistics = bloggersDataUpdater.updateData(bloggers)

        then:
        1 * blogRepository.save({
            it.bookmarkableId == bookmarkableId && it.author == newName && it.rss == existingPerson.rss &&
                it.twitter == existingPerson.twitter
        })
        statistics.getUpdated() == 1
    }

    def "Should update existing blog if url was changed"() {
        given:
        String newBlogUrl = 'http://new.blog.pl'
        String bookmarkableId = 'bookmarkableId-2207'
        Blog blog = buildBlog(bookmarkableId, 'author', RSS_OF_VALID_BLOG, 'http://old.blog.pl',
            'twitter', LocalDateTime.now(), PERSONAL
        )
        BloggerEntry entry = new BloggerEntry(
            blog.bookmarkableId, blog.author, blog.rss, newBlogUrl, blog.twitter, COMPANY
        )
        blogRepository.findByBookmarkableId(entry.bookmarkableId) >> Option.of(blog)
        BloggersData bloggers = buildBloggersData(entry)

        when:
        UpdateStatistic statistics = bloggersDataUpdater.updateData(bloggers)

        then:
        1 * blogRepository.save({
            it.url == 'http://new.blog.pl'
        })
        statistics.getUpdated() == 1
    }

    def "Should not update existing blog url if new address could not be retrieved"() {
        given:
        String bookmarkableId = 'bookmarkableId-2207'
        Blog blog = buildBlog(bookmarkableId,'author', RSS_OF_VALID_BLOG, 'http://old.blog.pl',
            'twitter', LocalDateTime.now(), PERSONAL
        )
        BloggerEntry entry = new BloggerEntry(
            blog.bookmarkableId, blog.author, blog.rss, blog.twitter, COMPANY
        )
        blogRepository.findByBookmarkableId(entry.bookmarkableId) >> Option.of(blog)
        BloggersData bloggers = buildBloggersData(entry)

        when:
        UpdateStatistic statistics = bloggersDataUpdater.updateData(bloggers)

        then:
        1 * blogRepository.save({
            it.url != ''
        })
        statistics.getUpdated() == 1
    }

    def "Should set moderation required if address is from medium"() {
        given:
        String rssFromMedium = "https://medium.com/feed/@user"
        String bookmarkableId = 'bookmarkableId-2207'
        BloggerEntry entry = buildBloggerEntry(bookmarkableId, 'blog', rssFromMedium, 'page', 'twitter', PERSONAL)
        blogRepository.findByBookmarkableId(bookmarkableId) >> Option.none()
        BloggersData bloggers = buildBloggersData(entry)

        when:
        UpdateStatistic statistics = bloggersDataUpdater.updateData(bloggers)

        then:
        1 * blogRepository.save({
            it.moderationRequired == true
        })
        statistics.getCreated() == 1
    }

    def "Should set moderation not required if blog is personal and not from medium"() {
        given:
        String bookmarkableId = 'bookmarkableId-2207'
        BloggerEntry entry = buildBloggerEntry(bookmarkableId, 'blog', RSS_OF_VALID_BLOG, 'page', 'twitter', PERSONAL)
        blogRepository.findByBookmarkableId(bookmarkableId) >> Option.none()
        BloggersData bloggers = buildBloggersData(entry)

        when:
        UpdateStatistic statistics = bloggersDataUpdater.updateData(bloggers)

        then:
        1 * blogRepository.save({
            it.moderationRequired == false
        })
        statistics.getCreated() == 1
    }

    def buildBloggersData(BloggerEntry bloggerEntry) {
        BloggersData bloggersData = new BloggersData()
        bloggersData.getBloggers().add(bloggerEntry)
        return bloggersData
    }

    def buildBlog(String bookmarkableId, String author, String rss, String pageUrl, String twitter) {
        buildBlog(bookmarkableId, author, rss, pageUrl, twitter, LocalDateTime.now(), PERSONAL)
    }

    def buildBlog(String bookmarkableId, String author, String rss, String url, String twitter, LocalDateTime dateAdded, BlogType type) {
        return Blog.builder()
            .bookmarkableId(bookmarkableId)
            .author(author)
            .rss(rss)
            .url(url)
            .twitter(twitter)
            .dateAdded(dateAdded)
            .blogType(type)
            .build()
    }

    def buildBloggerEntry(String bookmarkableId, String author, String rss, String pageUrl, String twitter, BlogType type) {
        return new BloggerEntry(bookmarkableId, author, rss, pageUrl, twitter, type)
    }

    def syndFeedProducer() {
        SyndFeedProducer producer = Stub(SyndFeedProducer)
        producer.validUrlFromRss('') >> Option.none()
        producer.validUrlFromRss(RSS_OF_VALID_BLOG) >> Option.of('http://new.blog.pl')
        return producer
    }
}
