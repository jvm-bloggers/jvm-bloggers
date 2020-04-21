package com.jvm_bloggers.core.data_fetching.blog_posts

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.JavaTestKit
import com.jvm_bloggers.TestTimeProvider
import com.jvm_bloggers.core.rss.SyndFeedProducer
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.utils.NowProvider
import com.jvm_bloggers.utils.ZoneTimeProvider
import com.rometools.rome.feed.synd.SyndEntry
import com.rometools.rome.feed.synd.SyndFeed
import io.vavr.control.Option
import scala.concurrent.duration.FiniteDuration
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime
import java.time.ZonedDateTime

@Subject(ActorRef)
class RssCheckingActorSpec extends Specification {

    static final Blog BLOG = Blog.builder()
        .bookmarkableId('tomasz-dziurko')
        .author('Tomasz Dziurko')
        .rss('http://tomaszdziurko.pl/feed/')
        .url('url')
        .dateAdded(new ZoneTimeProvider().now())
        .blogType(BlogType.PERSONAL)
        .build()

    JavaTestKit testProbe
    SyndFeedProducer syndFeedProducer
    ActorRef rssCheckingActor
    LocalDateTime DATE = new ZoneTimeProvider().now()
    NowProvider nowProvider = new TestTimeProvider(DATE)

    def setup() {
        ActorSystem system = ActorSystem.create('test')
        testProbe = new JavaTestKit(system)
        syndFeedProducer = Mock(SyndFeedProducer)
        Props props = RssCheckingActor.props(testProbe.getRef(), syndFeedProducer, nowProvider)
        rssCheckingActor = system.actorOf(props, 'rssCheckingActor')
    }

    def cleanup() {
        testProbe.system.terminate()
    }

    def "Should send message about new posts to postStoringActor"() {
        given:
        mockFeedToReturnNumberOfPosts(syndFeedProducer, 1, new Date())

        when:
        rssCheckingActor.tell(new RssLink(BLOG), ActorRef.noSender())

        then:
        testProbe.expectMsgClass(RssEntryWithAuthor)
    }

    def "Should not send any message about new posts to postStoringActor when there are no posts in the feed "() {
        given:
        mockFeedToReturnNumberOfPosts(syndFeedProducer, 0, new Date())

        when:
        rssCheckingActor.tell(new RssLink(BLOG), ActorRef.noSender())

        then:
        testProbe.expectNoMsg(FiniteDuration.apply(3, 'second'))
    }

    def "Should not send any message about post older than one week"() {
        given:
        Date nineDaysBeforeNow = Date.from(
                ZonedDateTime.now().minusDays(9).toInstant())
        mockFeedToReturnNumberOfPosts(syndFeedProducer, 1, nineDaysBeforeNow)

        when:
        rssCheckingActor.tell(new RssLink(BLOG), ActorRef.noSender())

        then:
        testProbe.expectNoMsg(FiniteDuration.apply(3, 'second'))
    }

    private void mockFeedToReturnNumberOfPosts(SyndFeedProducer factory, int numberOfPosts, Date publishedDate) {
        SyndFeed syndFeedMock = Mock(SyndFeed)
        syndFeedMock.getEntries() >> mockEntries(numberOfPosts, publishedDate)
        factory.createFor(_ as String) >> Option.of(syndFeedMock)
    }

    List<SyndEntry> mockEntries(int size, Date publishedDate) {
        List<SyndEntry> entries = []

        (0..<size).each {
            SyndEntry entry = Mock(SyndEntry)
            entry.title = "Title $it"
            entry.getPublishedDate() >> publishedDate
            entries.add(entry)
        }

        return entries
    }
}
