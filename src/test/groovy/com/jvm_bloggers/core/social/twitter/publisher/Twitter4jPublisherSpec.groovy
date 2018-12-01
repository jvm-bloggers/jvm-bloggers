package com.jvm_bloggers.core.social.twitter.publisher

import com.jvm_bloggers.entities.twitter.Tweet
import spock.lang.Specification
import spock.lang.Subject
import twitter4j.Status
import twitter4j.Twitter
import twitter4j.TwitterException

import static com.jvm_bloggers.core.social.twitter.publisher.TwitterPublisher.TwitterPublishingStatus.ERROR
import static com.jvm_bloggers.core.social.twitter.publisher.TwitterPublisher.TwitterPublishingStatus.SUCCESS
import static java.time.LocalDateTime.now

@Subject(Twitter4jPublisher)
class Twitter4jPublisherSpec extends Specification {

    TwitterClientFactory clientFactory = Stub(TwitterClientFactory)
    Twitter twitterClient = Stub(Twitter)
    Twitter4jPublisher publisher = new Twitter4jPublisher(clientFactory)

    def setup() {
        clientFactory.client >> twitterClient
    }

    def "should publish a new tweet successfully"() {
        given:
        twitterClient.updateStatus(_) >> Stub(Status)

        when:
        def publishingStatus = publisher.publish(tweet())

        then:
        publishingStatus == SUCCESS
    }

    def "should not publish a new tweet in case of errors"() {
        given:
        twitterClient.updateStatus(_) >> { throw new TwitterException("test error") }

        when:
        def publishingStatus = publisher.publish(tweet())

        then:
        publishingStatus == ERROR
    }

    private Tweet tweet() {
        new Tweet("Tests, tests everywhere!!! :)", now())
    }

}