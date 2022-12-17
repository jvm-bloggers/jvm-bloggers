package com.jvm_bloggers.core.social.twitter.publisher

import com.jvm_bloggers.entities.twitter.Tweet
import spock.lang.Specification
import spock.lang.Subject
import twitter4j.TwitterException
import twitter4j.v1.Status
import twitter4j.v1.StatusUpdate
import twitter4j.v1.TweetsResources
import twitter4j.v1.TwitterV1

import static com.jvm_bloggers.core.social.twitter.publisher.TwitterPublisher.TwitterPublishingStatus.ERROR
import static com.jvm_bloggers.core.social.twitter.publisher.TwitterPublisher.TwitterPublishingStatus.SUCCESS
import static java.time.LocalDateTime.now

@Subject(Twitter4jPublisher)
class Twitter4jPublisherSpec extends Specification {

    TwitterClientFactory clientFactory = Stub(TwitterClientFactory)
    TwitterV1 twitterClient = Stub(TwitterV1)
    Twitter4jPublisher publisher = new Twitter4jPublisher(clientFactory)

    def setup() {
        clientFactory.client >> twitterClient
    }

    def "should publish a new tweet successfully"() {
        given:
        def resources = Stub(TweetsResources)

        twitterClient.tweets() >> resources
        resources.updateStatus(_ as String) >> Stub(Status)

        when:
        def publishingStatus = publisher.publish(tweet())

        then:
        publishingStatus == SUCCESS
    }

    def "should not publish a new tweet in case of errors"() {
        given:
        def resources = Stub(TweetsResources)

        twitterClient.tweets() >> resources
        resources.updateStatus(_ as String) >> { throw new TwitterException("test error") }

        when:
        def publishingStatus = publisher.publish(tweet())

        then:
        publishingStatus == ERROR
    }

    private Tweet tweet() {
        new Tweet("Tests, tests everywhere!!! :)", now())
    }

}