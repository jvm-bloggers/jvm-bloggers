package com.jvm_bloggers.core.social.twitter.publisher

import com.jvm_bloggers.entities.twitter.Tweet
import spock.lang.Specification
import twitter4j.Status
import twitter4j.Twitter
import twitter4j.TwitterException

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
        publishingStatus == TwitterPublisher.TwitterPublishingStatus.SUCCESS
    }

    def "should not publish a new tweet in case of errors"() {
        given:
        twitterClient.updateStatus(_) >> { throw new TwitterException("test error") }

        when:
        def publishingStatus = publisher.publish(tweet())

        then:
        publishingStatus == TwitterPublisher.TwitterPublishingStatus.ERROR
    }

    private Tweet tweet() {
        new Tweet("Tests, tests everywhere!!! :)")
    }

}