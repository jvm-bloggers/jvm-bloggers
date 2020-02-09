package com.jvm_bloggers.core.rss

import com.rometools.rome.feed.synd.*

/**
 * Provides mocked {@link SyndFeed} instances
 */
class TestSyndFeedProvider {
    private Date today = new Date(0)

    def getSyndFeed() {
        SyndLink link = new SyndLinkImpl()
        link.rel = "self"
        link.href = "http://jvm-bloggers.com/pl/rss"

        SyndFeed feed = new SyndFeedImpl()
        feed.links = [link]
        feed.uri = "URI"
        feed.feedType = AggregatedRssFeedProducer.FEED_TYPE
        feed.title = AggregatedRssFeedProducer.FEED_TITLE
        feed.description = AggregatedRssFeedProducer.FEED_DESCRIPTION
        feed.publishedDate = today
        feed.entries = [createSyndEntry("postId", "postUrl", "postTitle", "postAuthor", "postDescription", today)]

        return feed
    }

    static createSyndEntry(String id, String url, String title, String author, String description, Date publishedDate) {
        SyndContent descriptionContent = new SyndContentImpl()
        descriptionContent.value = description

        SyndEntry entry = new SyndEntryImpl()
        entry.uri = id
        entry.link = url
        entry.title = title
        entry.author = author
        entry.description = descriptionContent
        entry.publishedDate = publishedDate

        return entry
    }
}
