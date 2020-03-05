package com.jvm_bloggers.core.social.twitter.publisher

import com.jvm_bloggers.TestTimeProvider
import com.jvm_bloggers.entities.twitter.Tweet
import com.jvm_bloggers.entities.twitter.TweetRepository
import com.jvm_bloggers.utils.NowProvider
import io.vavr.control.Option
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime

@Subject(TwitterPublishingScheduler)
class TwitterPublishingSchedulerSpec extends Specification {

    private static final LocalDateTime NOW = LocalDateTime.now()

    private TweetRepository tweetRepository = Mock(TweetRepository)
    private TwitterPublisher publisher = Mock(TwitterPublisher)
    private NowProvider nowProvider = new TestTimeProvider(NOW)

    TwitterPublishingScheduler publisherScheduler = new TwitterPublishingScheduler(tweetRepository, publisher, nowProvider)

    def "Should mark published tweet as sent"() {
        given:
        Tweet tweet = Mock(Tweet)
        tweetRepository.findFirstBySentIsFalseAndPostingDateLessThan(_) >> Option.of(tweet)
        publisher.publish(_) >> TwitterPublisher.TwitterPublishingStatus.SUCCESS

        when:
        publisherScheduler.publishOnePost()

        then:
        1 * tweet.markAsSent()
        1 * tweetRepository.save(tweet)
    }

    def "Should not execute any action when there are no tweets waiting for publication"() {
        given:
        tweetRepository.findFirstBySentIsFalseAndPostingDateLessThan(_) >> Option.none()

        when:
        publisherScheduler.publishOnePost()

        then:
        0 * publisher.publish(_)
        0 * tweetRepository.save(_ as Tweet)
    }

    def "Should not mark tweet as sent after unsuccessful sending action"() {
        given:
        Tweet tweet = Mock(Tweet)
        tweetRepository.findFirstBySentIsFalseAndPostingDateLessThan(_) >> Option.of(tweet)
        publisher.publish(_) >> TwitterPublisher.TwitterPublishingStatus.ERROR

        when:
        publisherScheduler.publishOnePost()

        then:
        0 * tweet.markAsSent()
        0 * tweetRepository.save(tweet)
    }
    
}