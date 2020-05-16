package com.jvm_bloggers.core.data_fetching.blog_posts

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.JavaTestKit
import com.jvm_bloggers.core.rss.SyndFeedProducer
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.utils.ZoneTimeProvider
import com.rometools.rome.feed.synd.SyndEntry
import com.rometools.rome.feed.synd.SyndFeed
import io.vavr.control.Option
import scala.concurrent.duration.FiniteDuration
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime

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

    def setup() {
        ActorSystem system = ActorSystem.create('test')
        testProbe = new JavaTestKit(system)
        syndFeedProducer = Mock(SyndFeedProducer)
        Props props = RssCheckingActor.props(testProbe.getRef(), syndFeedProducer)
        rssCheckingActor = system.actorOf(props, 'rssCheckingActor')
    }

    def cleanup() {
        testProbe.system.terminate()
    }

    def "Should send message about new posts to postStoringActor"() {
        given:
        mockFeedToReturnNumberOfPosts(syndFeedProducer, 1)

        when:
        rssCheckingActor.tell(new RssLink(BLOG), ActorRef.noSender())

        then:
        testProbe.expectMsgClass(RssEntryWithAuthor)
    }

    def "Should not send any message about new posts to postStoringActor when there are no posts in the feed "() {
        given:
        mockFeedToReturnNumberOfPosts(syndFeedProducer, 0)

        when:
        rssCheckingActor.tell(new RssLink(BLOG), ActorRef.noSender())

        then:
        testProbe.expectNoMsg(FiniteDuration.apply(3, 'second'))
    }

    private void mockFeedToReturnNumberOfPosts(SyndFeedProducer factory, int numberOfPosts) {
        SyndFeed syndFeedMock = Mock(SyndFeed)
        syndFeedMock.getEntries() >> mockEntries(numberOfPosts)
        factory.createFor(_ as String) >> Option.of(syndFeedMock)
    }

    List<SyndEntry> mockEntries(int size) {
        List<SyndEntry> entries = []

        (0..<size).each {
            SyndEntry entry = Mock(SyndEntry)
            entry.title = "Title $it"
            entries.add(entry)
        }

        return entries
    }
}
