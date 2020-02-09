package com.jvm_bloggers.entities.fb

import com.jvm_bloggers.SpringContextAwareSpecification
import io.vavr.control.Option
import org.springframework.beans.factory.annotation.Autowired
import spock.lang.Subject

import java.time.LocalDateTime

import static java.time.LocalDateTime.now

@Subject(FacebookPostRepository)
class FacebookPostRepositoryIntegrationSpec extends SpringContextAwareSpecification {

    @Autowired
    FacebookPostRepository facebookPostRepository

    def "Should persist FacebookPost entity"() {
        given:
        FacebookPost post = preparePost("http://jvm-bloggers.com/issues/1", now())

        when:
        facebookPostRepository.save(post)

        then:
        List<FacebookPost> allPosts = facebookPostRepository.findAll()
        allPosts.size() == 1
        allPosts.get(0).id == post.id
    }

    def "Should find one not sent FacebookPost"() {
        given:
        FacebookPost post1 = preparePost("http://jvm-bloggers.com/issues/1", now())
        FacebookPost post2 = preparePost("http://jvm-bloggers.com/issues/2", now())
        post2.markAsSent()
        and:
        facebookPostRepository.save(post1)
        facebookPostRepository.save(post2)

        when:
        Option<FacebookPost> notSentPost = facebookPostRepository.findFirstBySentIsFalseAndPostingDateLessThan(now().plusSeconds(1))

        then:
        notSentPost.isDefined()
        notSentPost.get().link == post1.link
    }

    def "Should find zero not sent posts "() {
        given:
        FacebookPost post1 = preparePost("http://jvm-bloggers.com/issues/1", now())
        post1.markAsSent()
        FacebookPost post2 = preparePost("http://jvm-bloggers.com/issues/2", now())
        post2.markAsSent()
        and:
        facebookPostRepository.save(post1)
        facebookPostRepository.save(post2)

        when:
        Option<FacebookPost> notSentPost = facebookPostRepository.findFirstBySentIsFalseAndPostingDateLessThan(now())

        then:
        notSentPost.isEmpty()
    }

    def "Should find zero posts if all unsent are scheduled in the future"() {
        given:
        FacebookPost post = preparePost("http://jvm-bloggers.com/issues/1", now().plusDays(1))
        facebookPostRepository.save(post)

        when:
        Option<FacebookPost> notSentPost = facebookPostRepository.findFirstBySentIsFalseAndPostingDateLessThan(now())

        then:
        notSentPost.isEmpty()
    }

    def "Should find one not sent post when second is scheduled in the future"() {
        given:
        FacebookPost post1 = preparePost("http://jvm-bloggers.com/issues/1", now())
        FacebookPost post2 = preparePost("http://jvm-bloggers.com/issues/2", now().plusDays(1))

        and:
        facebookPostRepository.save(post1)
        facebookPostRepository.save(post2)

        when:
        Option<FacebookPost> notSentPost = facebookPostRepository.findFirstBySentIsFalseAndPostingDateLessThan(now().plusSeconds(1))

        then:
        notSentPost.isDefined()
        notSentPost.get().id == post1.id
    }

    private FacebookPost preparePost(String link, LocalDateTime sentDate) {
        return new FacebookPost(
            link,
            "anyContent",
            sentDate
        )
    }

}