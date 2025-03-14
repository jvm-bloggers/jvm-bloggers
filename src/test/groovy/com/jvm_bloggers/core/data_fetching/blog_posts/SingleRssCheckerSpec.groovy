package com.jvm_bloggers.core.data_fetching.blog_posts

import com.jvm_bloggers.core.rss.SyndFeedProducer
import com.jvm_bloggers.entities.blog.BlogRepository
import com.rometools.rome.feed.synd.SyndEntry
import com.rometools.rome.feed.synd.SyndFeed
import io.vavr.control.Option
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime

import static com.jvm_bloggers.ObjectMother.aBlog

@Subject(SingleRssChecker)
class SingleRssCheckerSpec extends Specification {

    private SyndFeedProducer syndFeedFactory = Mock()
    private BlogPostService blogPostService = Mock()
    private BlogRepository blogRepository = Mock()

    SingleRssChecker singleRssChecker = new SingleRssChecker(syndFeedFactory, blogPostService, blogRepository)

    def "Should update post and last success date of RSS fetch"() {
        given:
        def blog = aBlog(id: 1L)

        when:
        singleRssChecker.checkForNewEntries(blog)

        then:
        1 * syndFeedFactory.createFor(blog.rss) >> Option.of(createSyndFeed())
        3 * blogPostService.addOrUpdate(_ as RssEntryWithAuthor)
        1 * blogRepository.updateDateLastFetched(_ as LocalDateTime, blog.id)
    }

    def "Should not update post and last success date of RSS fetch if feed is empty"() {
        given:
        def blog = aBlog(id: 1L)

        when:
        singleRssChecker.checkForNewEntries(blog)

        then:
        1 * syndFeedFactory.createFor(blog.rss) >> Option.none()
        0 * blogPostService.addOrUpdate(_ as RssEntryWithAuthor)
        0 * blogRepository.updateDateLastFetched(_ as LocalDateTime, blog.id)
    }

    SyndFeed createSyndFeed() {
        def syndFeed = Mock(SyndFeed)
        syndFeed.entries >> [Mock(SyndEntry), Mock(SyndEntry), Mock(SyndEntry)]
        syndFeed
    }
}
