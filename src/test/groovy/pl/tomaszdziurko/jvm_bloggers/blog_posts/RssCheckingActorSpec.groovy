package pl.tomaszdziurko.jvm_bloggers.blog_posts

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.JavaTestKit
import com.sun.syndication.feed.synd.SyndEntry
import com.sun.syndication.feed.synd.SyndFeed
import com.sun.syndication.io.SyndFeedInput
import com.sun.syndication.io.XmlReader
import pl.tomaszdziurko.jvm_bloggers.people.domain.Person
import scala.concurrent.duration.FiniteDuration
import spock.lang.Specification
import spock.lang.Subject

class RssCheckingActorSpec extends Specification {

    JavaTestKit testProbe
    SyndFeedInput syncFeedInput

    @Subject
    ActorRef rssCheckingActor

    def setup() {
        ActorSystem system = ActorSystem.create("test")
        testProbe = new JavaTestKit(system);
        syncFeedInput = Mock(SyndFeedInput)
        Props props = RssCheckingActor.props(testProbe.getRef(), syncFeedInput)
        rssCheckingActor = system.actorOf(props, "rssCheckingActor")
    }

    def cleanup() {
        testProbe.system.shutdown()
    }

    def "Should send message about new posts to postStoringActor"() {
        given:
            mockFeedToReturnNumberOfPosts(syncFeedInput, 1)
        when:
            rssCheckingActor.tell(new RssLink(new Person(name: "Tomasz Dziurko", rss: "http://tomaszdziurko.pl/feed/")), ActorRef.noSender())
        then:
            testProbe.expectMsgClass(RssEntryWithAuthor)
    }

    def "Should not send any message about new posts to postStoringActor when there are no posts in the feed "() {
        given:
            mockFeedToReturnNumberOfPosts(syncFeedInput, 0)
        when:
            rssCheckingActor.tell(new RssLink(new Person(name: "Tomasz Dziurko", rss: "http://tomaszdziurko.pl/feed/")), ActorRef.noSender())
        then:
            testProbe.expectNoMsg(FiniteDuration.apply(3, "second"))
    }

    private void mockFeedToReturnNumberOfPosts(SyndFeedInput input, int numberOfPosts) {
        SyndFeed syndFeedMock = Mock(SyndFeed)
        syndFeedMock.getEntries() >> mockEntries(numberOfPosts)
        input.build(_ as XmlReader) >> syndFeedMock
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
