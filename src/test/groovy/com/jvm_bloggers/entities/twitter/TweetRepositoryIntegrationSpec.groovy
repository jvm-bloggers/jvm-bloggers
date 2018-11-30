package com.jvm_bloggers.entities.twitter

import com.jvm_bloggers.SpringContextAwareSpecification
import io.vavr.control.Option
import org.springframework.beans.factory.annotation.Autowired
import spock.lang.Shared
import spock.lang.Subject

import java.time.LocalDateTime

@Subject(TweetRepository)
class TweetRepositoryIntegrationSpec extends SpringContextAwareSpecification {

    @Autowired
    TweetRepository tweetRepository

    @Shared
    LocalDateTime NOW = LocalDateTime.now()

    def "Should persist Tweet entity"() {
        given:
        Tweet tweet = prepareTweet("tweet content #1", NOW)

        when:
        tweetRepository.save(tweet)

        then:
        List<Tweet> tweets = tweetRepository.findAll()
        tweets.size() == 1
        tweets.get(0).id == tweet.id
    }

    def "Should find one not sent Tweet"() {
        given:
        Tweet tweet1 = prepareTweet("tweet content #1", NOW)
        Tweet tweet2 = prepareTweet("tweet content #2", NOW)
        tweet2.markAsSent()

        and:
        tweetRepository.save(tweet1)
        tweetRepository.save(tweet2)

        when:
        Option<Tweet> notSentTweet = tweetRepository.findFirstBySentIsFalseAndPostingDateLessThan(NOW.plusSeconds(1))

        then:
        notSentTweet.isDefined()
        notSentTweet.get().content == tweet1.content
    }

    def "Should find zero not sent tweets when all tweets were already sent"() {
        given:
        Tweet tweet1 = prepareTweet("tweet content #1", NOW)
        tweet1.markAsSent()
        Tweet tweet2 = prepareTweet("tweet content #2", NOW)
        tweet2.markAsSent()
        and:
        tweetRepository.save(tweet1)
        tweetRepository.save(tweet2)

        when:
        Option<Tweet> notSentTweet = tweetRepository.findFirstBySentIsFalseAndPostingDateLessThan(NOW.plusSeconds(1))

        then:
        notSentTweet.isEmpty()
    }

    def "Should find one not sent tweets when second is scheduled in the future"() {
        given:
        Tweet tweet1 = prepareTweet("tweet content #1", NOW)
        Tweet tweet2 = prepareTweet("tweet content #2", NOW.plusDays(1))
        and:
        tweetRepository.save(tweet1)
        tweetRepository.save(tweet2)

        when:
        Option<Tweet> notSentTweet = tweetRepository.findFirstBySentIsFalseAndPostingDateLessThan(NOW.plusSeconds(1))

        then:
        notSentTweet.isDefined()
        notSentTweet.get().id == tweet1.id
    }

    private Tweet prepareTweet(String content, LocalDateTime sentDate) {
        return new Tweet(content, sentDate)
    }

}