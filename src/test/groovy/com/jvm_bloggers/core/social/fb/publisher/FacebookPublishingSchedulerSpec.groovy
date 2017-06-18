package com.jvm_bloggers.core.social.fb.publisher

import com.jvm_bloggers.TestNowProvider
import com.jvm_bloggers.entities.fb.FacebookPost
import com.jvm_bloggers.entities.fb.FacebookPostRepository
import com.jvm_bloggers.utils.NowProvider
import javaslang.control.Option
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime

class FacebookPublishingSchedulerSpec extends Specification {

    private static final LocalDateTime NOW = LocalDateTime.now();

    private final FacebookPostRepository fbPostRepository = Mock(FacebookPostRepository)
    private final FacebookPublisher fbPublisher = Mock(FacebookPublisher)
    private final NowProvider nowProvider = new TestNowProvider(NOW)

    @Subject
    FacebookPublishingScheduler fbPublisherScheduler = new FacebookPublishingScheduler(fbPostRepository, fbPublisher, nowProvider)

    def "Should save published FB post with set sentDate"() {
        given:
        FacebookPost post = Mock(FacebookPost)
        fbPostRepository.findFirstBySentDateNull() >> Option.of(post)
        fbPublisher.publishPost(_) >> FacebookPublisher.FacebookPublishingStatus.SUCCESS

        when:
        fbPublisherScheduler.publishOnePost()

        then:
        1 * post.setSentDate(NOW)
        1 * fbPostRepository.save(post)
    }

    def "Should not execute any action for zero not published FB posts"() {
        given:
        fbPostRepository.findFirstBySentDateNull() >> Option.none()

        when:
        fbPublisherScheduler.publishOnePost()

        then:
        0 * fbPublisher.publishPost(_)
        0 * fbPostRepository.save(_ as FacebookPost)
    }

    def "Should not update sentDate for unsuccessful sending action"() {
        given:
        FacebookPost post = Mock(FacebookPost)
        fbPostRepository.findFirstBySentDateNull() >> Option.of(post)
        fbPublisher.publishPost(_) >> FacebookPublisher.FacebookPublishingStatus.ERROR

        when:
        fbPublisherScheduler.publishOnePost()

        then:
        0 * post.setSentDate(NOW)
        0 * fbPostRepository.save(post)
    }

}