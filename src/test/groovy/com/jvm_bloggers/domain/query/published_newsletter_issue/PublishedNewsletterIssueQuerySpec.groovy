package com.jvm_bloggers.domain.query.published_newsletter_issue

import com.jvm_bloggers.domain.query.NewsletterIssueNumber
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository
import javaslang.control.Option
import spock.lang.Specification
import spock.lang.Subject

class PublishedNewsletterIssueQuerySpec extends Specification {

    NewsletterIssueRepository newsletterIssueRepository = Mock(NewsletterIssueRepository)
    PublishedNewsletterIssueBuilder publishedNewsletterIssueBuilder = Mock(PublishedNewsletterIssueBuilder)

    @Subject
    PublishedNewsletterIssueQuery publishedNewsletterIssueQuery = new PublishedNewsletterIssueQuery(
            newsletterIssueRepository, publishedNewsletterIssueBuilder)

    def "Should find previous and next issue numbers"() {
        given:
        NewsletterIssueNumber issueNumber = NewsletterIssueNumber.of(22L)
        NewsletterIssueNumber previous = NewsletterIssueNumber.previous(issueNumber.asLong())
        NewsletterIssueNumber next = NewsletterIssueNumber.next(issueNumber.asLong())
        newsletterIssueRepository.issueNumberExist(previous.asLong()) >> true
        newsletterIssueRepository.issueNumberExist(next.asLong()) >> true

        when:
        Option<NewsletterIssueNumber> resultPreviousIssueNumber = publishedNewsletterIssueQuery.findPreviousIssueNumber(issueNumber)
        Option<NewsletterIssueNumber> resultNextIssueNumber = publishedNewsletterIssueQuery.findNextIssueNumber(issueNumber)

        then:
        resultPreviousIssueNumber.isDefined()
        resultPreviousIssueNumber.get() == previous
        resultNextIssueNumber.isDefined()
        resultNextIssueNumber.get() == next
    }

    def "Should not find previous and next issue numbers"() {
        given:
        NewsletterIssueNumber issueNumber = NewsletterIssueNumber.of(22L)
        NewsletterIssueNumber previous = NewsletterIssueNumber.previous(issueNumber.asLong())
        NewsletterIssueNumber next = NewsletterIssueNumber.next(issueNumber.asLong())
        newsletterIssueRepository.issueNumberExist(previous.asLong()) >> false
        newsletterIssueRepository.issueNumberExist(next.asLong()) >> false

        when:
        Option<NewsletterIssueNumber> resultPreviousIssueNumber = publishedNewsletterIssueQuery.findPreviousIssueNumber(issueNumber)
        Option<NewsletterIssueNumber> resultNextIssueNumber = publishedNewsletterIssueQuery.findNextIssueNumber(issueNumber)

        then:
        !resultPreviousIssueNumber.isDefined()
        !resultNextIssueNumber.isDefined()
    }

}