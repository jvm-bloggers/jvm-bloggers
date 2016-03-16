package pl.tomaszdziurko.jvm_bloggers.blog_posts

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.JavaTestKit
import com.rometools.rome.feed.synd.SyndEntry
import com.rometools.rome.feed.synd.SyndFeed
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.utils.SyndFeedProducer
import scala.concurrent.duration.FiniteDuration
import spock.lang.Specification
import spock.lang.Subject

class RssCheckingActorSpec extends Specification {

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
            rssCheckingActor.tell(new RssLink(new Blog(author: "Tomasz Dziurko", rss: "http://tomaszdziurko.pl/feed/")), ActorRef.noSender())
        then:
            testProbe.expectMsgClass(RssEntryWithAuthor)
    }

    def "Should not send any message about new posts to postStoringActor when there are no posts in the feed "() {
        given:
            mockFeedToReturnNumberOfPosts(syndFeedProducer, 0)
        when:
            rssCheckingActor.tell(new RssLink(new Blog(author: "Tomasz Dziurko", rss: "http://tomaszdziurko.pl/feed/")), ActorRef.noSender())
        then:
            testProbe.expectNoMsg(FiniteDuration.apply(3, "second"))
    }

    private void mockFeedToReturnNumberOfPosts(SyndFeedProducer factory, int numberOfPosts) {
        SyndFeed syndFeedMock = Mock(SyndFeed)
        syndFeedMock.getEntries() >> mockEntries(numberOfPosts)
        factory.createFor(_ as String) >> syndFeedMock
    }

    List<SyndEntry> mockEntries(int size) {
        List<SyndEntry> entries = []

        for (int i = 0; i < size; i++) {
            SyndEntry entry = Mock(SyndEntry)
            entry.title = "Title $i"
            entries.add(entry)
        }
        return entries
    }
}
