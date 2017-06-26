package com.jvm_bloggers.entities.twitter

import com.jvm_bloggers.SpringContextAwareSpecification
import javaslang.control.Option
import org.springframework.beans.factory.annotation.Autowired

import java.time.LocalDateTime

class TweetRepositoryIntegrationSpec extends SpringContextAwareSpecification {

    @Autowired
    TweetRepository tweetRepository

    def "Should persist Tweet entity"() {
        given:
        Tweet tweet = prepareTweet("tweet content #1")

        when:
        tweetRepository.save(tweet)

        then:
        List<Tweet> tweets = tweetRepository.findAll()
        tweets.size() == 1
        tweets.get(0).id == tweet.id
    }

    def "Should find one not sent Tweet"() {
        given:
        Tweet tweet1 = prepareTweet("tweet content #1")
        Tweet tweet2 = prepareTweet("tweet content #2")
        tweet2.setSentDate(LocalDateTime.now())
        and:
        tweetRepository.save(tweet1)
        tweetRepository.save(tweet2)

        when:
        Option<Tweet> notSentTweet = tweetRepository.findFirstBySentDateNull()

        then:
        notSentTweet.isDefined()
        notSentTweet.get().content == tweet1.content
    }

    def "Should find zero not sent tweets"() {
        given:
        Tweet tweet1 = prepareTweet("tweet content #1")
        tweet1.setSentDate(LocalDateTime.now())
        Tweet tweet2 = prepareTweet("tweet content #2")
        tweet2.setSentDate(LocalDateTime.now())
        and:
        tweetRepository.save(tweet1)
        tweetRepository.save(tweet2)

        when:
        Option<Tweet> notSentTweet = tweetRepository.findFirstBySentDateNull()

        then:
        notSentTweet.isEmpty()
    }

    private Tweet prepareTweet(String content) {
        return new Tweet(content)
    }

}