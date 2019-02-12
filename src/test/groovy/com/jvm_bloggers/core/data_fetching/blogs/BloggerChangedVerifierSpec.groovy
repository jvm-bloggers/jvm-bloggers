package com.jvm_bloggers.core.data_fetching.blogs

import com.jvm_bloggers.core.data_fetching.blogs.json_data.BloggerEntry
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogType
import spock.lang.Specification
import spock.lang.Subject
import spock.lang.Unroll

import java.time.LocalDateTime

import static BlogType.COMPANY
import static BlogType.PERSONAL

@Subject(BloggerChangedVerifier)
class BloggerChangedVerifierSpec extends Specification {

    BloggerChangedVerifier testObj = new BloggerChangedVerifier();

    @Unroll
    def "Should detect that BloggerEntry is different than corresponding Blog"() {
        when:
        boolean pendingChanges = testObj.pendingChanges(person, bloggerEntry)

        then:
        pendingChanges == expectedResult

        where:
        person                                 | bloggerEntry                | expectedResult
        standardPersonalBlog()                 | entryWithStandardBlogData() | false
        blogWithUppercasedRss()                | entryWithStandardBlogData() | false
        standardPersonalBlog()                 | entryWithCompanyBlogData()  | true
        blogWithDifferentAuthor()              | entryWithStandardBlogData() | true
        blogWithDifferentBookmarkableId()      | entryWithStandardBlogData() | true
        blogWithDifferentRss()                 | entryWithStandardBlogData() | true
        blogWithDifferentTwitter()             | entryWithStandardBlogData() | true
        blogWithDifferentAuthorRssAndTwitter() | entryWithStandardBlogData() | true
        standardPersonalBlog()                 | entryWithDifferentPage()    | true
    }

    private BloggerEntry entryWithCompanyBlogData() {
        buildBloggerEntry('bookmarkableId', 'blog', 'rss', 'page', 'twitter', COMPANY)
    }

    private BloggerEntry entryWithDifferentPage() {
        buildBloggerEntry('bookmarkableId', 'blog', 'rss', "newPage", 'twitter', PERSONAL)
    }

    private Blog standardPersonalBlog() {
        buildBlog('bookmarkableId', 'blog', 'rss', 'page', 'twitter')
    }

    private BloggerEntry entryWithStandardBlogData() {
        buildBloggerEntry('bookmarkableId', 'blog', 'rss', 'page', 'twitter', PERSONAL)
    }

    private Blog blogWithDifferentAuthorRssAndTwitter() {
        buildBlog('bookmarkableId',"authoX", "rsX", 'page', "twitteX")
    }

    private Blog blogWithDifferentBookmarkableId() {
        buildBlog("bookmarkableIdX","author", 'rss', 'page', 'twitter')
    }

    private Blog blogWithDifferentTwitter() {
        buildBlog('bookmarkableId', 'blog', 'rss', 'page', "twitterX")
    }

    private Blog blogWithDifferentRss() {
        buildBlog('bookmarkableId', 'blog', "Xss", 'page', 'twitter')
    }

    private Blog blogWithDifferentAuthor() {
        buildBlog('bookmarkableId', "Author", 'rss', 'page', 'twitter')
    }

    private Blog blogWithUppercasedRss() {
        buildBlog('bookmarkableId', 'blog', 'rss', 'page', 'twitter')
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
}
