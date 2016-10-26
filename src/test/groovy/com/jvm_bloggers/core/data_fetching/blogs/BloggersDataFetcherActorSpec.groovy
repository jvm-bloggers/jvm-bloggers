package com.jvm_bloggers.core.data_fetching.blogs

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.JavaTestKit
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogType
import scala.concurrent.duration.FiniteDuration
import spock.lang.Specification
import spock.lang.Subject

/**
 * Created by tgelo on 26.10.16.
 */
public class BloggersDataFetcherActorSpec extends Specification {

    @Subject
    ActorRef bloggersDataFetcherActor

    BloggersDataUpdater bloggersDataUpdater
    JavaTestKit testProbe

    def setup() {
        ActorSystem system = ActorSystem.create("test")
        testProbe = new JavaTestKit(system)
        bloggersDataUpdater = Mock(BloggersDataUpdater)
        Props props = BloggersDataFetcherActor.props(bloggersDataUpdater)
        bloggersDataFetcherActor = system.actorOf(props)
    }

    def "Should not throw exception when url is empty"() {
        given:
            BloggersUrlWithType bloggersUrlWithType = new BloggersUrlWithType(Optional.empty(), BlogType.COMPANY);
        when:
            bloggersDataFetcherActor.tell(bloggersUrlWithType, ActorRef.noSender())
        then:
            notThrown Exception
    }

    def "Should update bloggers data from valid url"() {
        given:
            String urlString = getClass().getResource("test_bloggers.json").toExternalForm()
            String urlString2 = getClass().getResource("test_companies.json").toExternalForm()
            String urlString3 = getClass().getResource("test_videos.json").toExternalForm()
            BloggersUrlWithType bloggersUrl1WithType = new BloggersUrlWithType(Optional.of(new URL(urlString)), BlogType.PERSONAL);
            BloggersUrlWithType bloggersUrl2WithType = new BloggersUrlWithType(Optional.of(new URL(urlString2)), BlogType.COMPANY);
            BloggersUrlWithType bloggersUrl3WithType = new BloggersUrlWithType(Optional.of(new URL(urlString3)), BlogType.VIDEOS);

        when:
            bloggersDataFetcherActor.tell(bloggersUrl1WithType, ActorRef.noSender())
            bloggersDataFetcherActor.tell(bloggersUrl2WithType, ActorRef.noSender())
            bloggersDataFetcherActor.tell(bloggersUrl3WithType, ActorRef.noSender())
            testProbe.expectNoMsg(FiniteDuration.apply(1, "second"))
        then:
            1 * bloggersDataUpdater.updateData({ it.bloggers.size == 1 })
            1 * bloggersDataUpdater.updateData({ it.bloggers.size == 2 })
            1 * bloggersDataUpdater.updateData({ it.bloggers.size == 1 })
    }
}
