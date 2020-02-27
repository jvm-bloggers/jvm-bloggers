package com.jvm_bloggers.core.social.fb.publisher

import com.jvm_bloggers.TestTimeProvider
import com.jvm_bloggers.entities.fb.FacebookPost
import com.jvm_bloggers.entities.fb.FacebookPostRepository
import com.jvm_bloggers.utils.NowProvider
import io.vavr.control.Option
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime

import static com.jvm_bloggers.core.social.fb.publisher.FacebookPublisher.FacebookPublishingStatus.ERROR
import static com.jvm_bloggers.core.social.fb.publisher.FacebookPublisher.FacebookPublishingStatus.SUCCESS

@Subject(FacebookPublishingScheduler)
class FacebookPublishingSchedulerSpec extends Specification {

    private static final LocalDateTime NOW = LocalDateTime.now()

    private FacebookPostRepository fbPostRepository = Mock(FacebookPostRepository)
    private FacebookPublisher fbPublisher = Mock(FacebookPublisher)
    private NowProvider nowProvider = new TestTimeProvider(NOW)

    FacebookPublishingScheduler fbPublisherScheduler = new FacebookPublishingScheduler(fbPostRepository, fbPublisher, nowProvider)

    def "Should mark published FB post as sent"() {
        given:
        FacebookPost post = Mock(FacebookPost)
        fbPostRepository.findFirstBySentIsFalseAndPostingDateLessThan(_) >> Option.of(post)
        fbPublisher.publishPost(_) >> SUCCESS

        when:
        fbPublisherScheduler.publishOnePost()

        then:
        1 * post.markAsSent()
        1 * fbPostRepository.save(post)
    }

    def "Should not execute any action for zero not published FB posts"() {
        given:
        fbPostRepository.findFirstBySentIsFalseAndPostingDateLessThan(_) >> Option.none()

        when:
        fbPublisherScheduler.publishOnePost()

        then:
        0 * fbPublisher.publishPost(_)
        0 * fbPostRepository.save(_ as FacebookPost)
    }

    def "Should not update sentDate after unsuccessful sending action"() {
        given:
        FacebookPost post = Mock(FacebookPost)
        fbPostRepository.findFirstBySentIsFalseAndPostingDateLessThan(_) >> Option.of(post)
        fbPublisher.publishPost(_) >> ERROR

        when:
        fbPublisherScheduler.publishOnePost()

        then:
        0 * post.markAsSent()
        0 * fbPostRepository.save(post)
    }

}