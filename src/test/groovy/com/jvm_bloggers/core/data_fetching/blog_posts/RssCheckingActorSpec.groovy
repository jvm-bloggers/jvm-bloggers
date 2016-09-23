package com.jvm_bloggers.core.data_fetching.blog_posts

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.JavaTestKit
import com.rometools.rome.feed.synd.SyndEntry
import com.rometools.rome.feed.synd.SyndFeed
import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogType;
import com.jvm_bloggers.utils.NowProvider
import com.jvm_bloggers.core.rss.SyndFeedProducer
import scala.concurrent.duration.FiniteDuration
import spock.lang.Specification
import spock.lang.Subject

class RssCheckingActorSpec extends Specification {

    static final Blog BLOG = Blog.builder()
        .jsonId(0L)
        .author("Tomasz Dziurko")
        .rss("http://tomaszdziurko.pl/feed/")
        .url("url")
        .dateAdded(new NowProvider().now())
        .blogType(BlogType.PERSONAL)
        .build();
        
    JavaTestKit testProbe
    SyndFeedProducer syndFeedProducer

    @Subject
    ActorRef rssCheckingActor

    def setup() {
        ActorSystem system = ActorSystem.create("test")
        testProbe = new JavaTestKit(system);
        syndFeedProducer = Mock(SyndFeedProducer)
        Props props = RssCheckingActor.props(testProbe.getRef(), syndFeedProducer)
        rssCheckingActor = system.actorOf(props, "rssCheckingActor")
    }

    def cleanup() {
        testProbe.system.shutdown()
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
            testProbe.expectNoMsg(FiniteDuration.apply(3, "second"))
    }

    private void mockFeedToReturnNumberOfPosts(SyndFeedProducer factory, int numberOfPosts) {
        SyndFeed syndFeedMock = Mock(SyndFeed)
        syndFeedMock.getEntries() >> mockEntries(numberOfPosts)
        factory.createFor(_ as String) >> Optional.of(syndFeedMock)
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
