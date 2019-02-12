package com.jvm_bloggers.domain.query.published_newsletter_issue

import com.jvm_bloggers.domain.query.NewsletterIssueNumber
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository
import io.vavr.control.Option
import spock.lang.Specification
import spock.lang.Subject

@Subject(PublishedNewsletterIssueQuery)
class PublishedNewsletterIssueQuerySpec extends Specification {

    NewsletterIssueRepository newsletterIssueRepository = Mock(NewsletterIssueRepository)
    PublishedNewsletterIssueBuilder publishedNewsletterIssueBuilder = Mock(PublishedNewsletterIssueBuilder)

    PublishedNewsletterIssueQuery publishedNewsletterIssueQuery = new PublishedNewsletterIssueQuery(
            newsletterIssueRepository, publishedNewsletterIssueBuilder)

    def "Should find previous issue numbers"() {
        given:
        NewsletterIssueNumber issueNumber = NewsletterIssueNumber.of(22L)
        NewsletterIssueNumber previous = NewsletterIssueNumber.previous(issueNumber)
        newsletterIssueRepository.existsByIssueNumber(previous.asLong()) >> true

        when:
        Option<NewsletterIssueNumber> resultPreviousIssueNumber = publishedNewsletterIssueQuery.findPreviousIssueNumber(issueNumber)

        then:
        resultPreviousIssueNumber.isDefined()
        resultPreviousIssueNumber.get() == previous
    }

    def "Should find next issue numbers"() {
        given:
        NewsletterIssueNumber issueNumber = NewsletterIssueNumber.of(22L)
        NewsletterIssueNumber next = NewsletterIssueNumber.next(issueNumber)
        newsletterIssueRepository.existsByIssueNumber(next.asLong()) >> true

        when:
        Option<NewsletterIssueNumber> resultNextIssueNumber = publishedNewsletterIssueQuery.findNextIssueNumber(issueNumber)

        then:
        resultNextIssueNumber.isDefined()
        resultNextIssueNumber.get() == next
    }

    def "Should not find previous issue numbers"() {
        given:
        NewsletterIssueNumber issueNumber = NewsletterIssueNumber.of(22L)
        NewsletterIssueNumber previous = NewsletterIssueNumber.previous(issueNumber)
        newsletterIssueRepository.existsByIssueNumber(previous.asLong()) >> false

        when:
        Option<NewsletterIssueNumber> resultPreviousIssueNumber = publishedNewsletterIssueQuery.findPreviousIssueNumber(issueNumber)

        then:
        resultPreviousIssueNumber.isEmpty()
    }

    def "Should not find next issue numbers"() {
        given:
        NewsletterIssueNumber issueNumber = NewsletterIssueNumber.of(22L)
        NewsletterIssueNumber next = NewsletterIssueNumber.next(issueNumber)
        newsletterIssueRepository.existsByIssueNumber(next.asLong()) >> false

        when:
        Option<NewsletterIssueNumber> resultNextIssueNumber = publishedNewsletterIssueQuery.findNextIssueNumber(issueNumber)

        then:
        resultNextIssueNumber.isEmpty()
    }

}