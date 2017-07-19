package com.jvm_bloggers.entities.fb

import com.jvm_bloggers.SpringContextAwareSpecification
import javaslang.control.Option
import org.springframework.beans.factory.annotation.Autowired

import java.time.LocalDateTime

class FacebookPostRepositoryIntegrationSpec extends SpringContextAwareSpecification {

    @Autowired
    FacebookPostRepository facebookPostRepository

    def "Should persist FacebookPost entity"() {
        given:
        FacebookPost post = preparePost("http://jvm-bloggers.com/issues/1")

        when:
        facebookPostRepository.save(post)

        then:
        List<FacebookPost> allPosts = facebookPostRepository.findAll()
        allPosts.size() == 1
        allPosts.get(0).id == post.id
    }

    def "Should find one not sent FacebookPost"() {
        given:
        FacebookPost post1 = preparePost("http://jvm-bloggers.com/issues/1")
        FacebookPost post2 = preparePost("http://jvm-bloggers.com/issues/2")
        post2.setSentDate(LocalDateTime.now())
        and:
        facebookPostRepository.save(post1)
        facebookPostRepository.save(post2)

        when:
        Option<FacebookPost> notSentPost = facebookPostRepository.findFirstBySentDateNull()

        then:
        notSentPost.isDefined()
        notSentPost.get().link == post1.link
    }

    def "Should find zero not sent posts "() {
        given:
        FacebookPost post1 = preparePost("http://jvm-bloggers.com/issues/1")
        post1.setSentDate(LocalDateTime.now())
        FacebookPost post2 = preparePost("http://jvm-bloggers.com/issues/2")
        post2.setSentDate(LocalDateTime.now())
        and:
        facebookPostRepository.save(post1)
        facebookPostRepository.save(post2)

        when:
        Option<FacebookPost> notSentPost = facebookPostRepository.findFirstBySentDateNull()

        then:
        notSentPost.isEmpty()
    }

    private FacebookPost preparePost(String link) {
        return new FacebookPost(
                link,
                "anyContent"
        )
    }

}