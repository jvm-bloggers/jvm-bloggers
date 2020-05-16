package com.jvm_bloggers.core.social.fb


import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator
import com.jvm_bloggers.core.newsletter_issues.NewIssuePublished
import com.jvm_bloggers.entities.fb.FacebookPostRepository
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue
import com.jvm_bloggers.utils.ZoneTimeProvider
import spock.lang.Specification
import spock.lang.Subject

@Subject(FacebookPostProducer)
class FacebookPostProducerSpec extends Specification {

    private static final String LINK_BASE = "http://jvm-bloggers.com/issues/"

    def "Should save a new Facebook post for a given issue"() {
        given:
        FacebookMessageGenerator messageGenerator = new FacebookMessageGenerator()
        LinkGenerator linkGenerator = Mock(LinkGenerator)
        linkGenerator.generateIssueLink(_) >> { args -> "$LINK_BASE${args[0]}" }
        FacebookPostRepository postRepository = Mock(FacebookPostRepository)

        and:
        NewIssuePublished issuePublishedEvent = new NewIssuePublished(
                NewsletterIssue
                        .builder()
                        .issueNumber(1L)
                        .heading("issue heading")
                        .build()
        )
        and:
        FacebookPostProducer facebookPostProducer = new FacebookPostProducer(linkGenerator, messageGenerator, postRepository, new ZoneTimeProvider())

        when:
        facebookPostProducer.handleNewIssueEvent(issuePublishedEvent)

        then:
        1 * postRepository.save({ it.link.startsWith(LINK_BASE) && it.message != null })
    }

}