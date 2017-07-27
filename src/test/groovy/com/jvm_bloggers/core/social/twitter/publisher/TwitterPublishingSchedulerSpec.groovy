package com.jvm_bloggers.core.social.twitter.publisher

import com.jvm_bloggers.TestNowProvider
import com.jvm_bloggers.entities.twitter.Tweet
import com.jvm_bloggers.entities.twitter.TweetRepository
import com.jvm_bloggers.utils.NowProvider
import io.vavr.control.Option
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime

class TwitterPublishingSchedulerSpec extends Specification {

    private static final LocalDateTime NOW = LocalDateTime.now();

    private final TweetRepository tweetRepository = Mock(TweetRepository)
    private final TwitterPublisher publisher = Mock(TwitterPublisher)
    private final NowProvider nowProvider = new TestNowProvider(NOW)

    @Subject
    TwitterPublishingScheduler publisherScheduler = new TwitterPublishingScheduler(tweetRepository, publisher, nowProvider)

    def "Should save published tweet with set sentDate"() {
        given:
            Tweet tweet = Mock(Tweet)
            tweetRepository.findFirstBySentDateNull() >> Option.of(tweet)
            publisher.publish(_) >> TwitterPublisher.TwitterPublishingStatus.SUCCESS

        when:
            publisherScheduler.publishOnePost()

        then:
            1 * tweet.setSentDate(NOW)
            1 * tweetRepository.save(tweet)
    }

    def "Should not execute any action for zero not published tweets"() {
        given:
            tweetRepository.findFirstBySentDateNull() >> Option.none()

        when:
            publisherScheduler.publishOnePost()

        then:
            0 * publisher.publish(_)
            0 * tweetRepository.save(_ as Tweet)
    }

    def "Should not update sentDate for unsuccessful sending action"() {
        given:
            Tweet tweet = Mock(Tweet)
            tweetRepository.findFirstBySentDateNull() >> Option.of(tweet)
            publisher.publish(_) >> TwitterPublisher.TwitterPublishingStatus.ERROR

        when:
            publisherScheduler.publishOnePost()

        then:
            0 * tweet.setSentDate(NOW)
            0 * tweetRepository.save(tweet)
    }
    
}