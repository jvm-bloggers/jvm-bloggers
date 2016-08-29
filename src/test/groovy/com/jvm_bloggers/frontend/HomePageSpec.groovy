package com.jvm_bloggers.frontend

import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssueDto
import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssueDtoService
import com.jvm_bloggers.frontend.newsletter_issue.newsletter_panel.NewsletterIssuePanel
import org.apache.wicket.markup.html.basic.Label
import com.jvm_bloggers.MockSpringContextAwareSpecification

import java.time.LocalDate


class HomePageSpec extends MockSpringContextAwareSpecification {

    NewsletterIssueDtoService newsletterIssueService = Stub(NewsletterIssueDtoService)


    @Override
    protected void setupContext() {
        addBean(newsletterIssueService)
    }

    def "Should display latest issue"() {
        given:
            NewsletterIssueDto issue = prepareExampleIssue()
            newsletterIssueService.getLatestIssue() >> Optional.of(issue)
        when:
            tester.startPage(HomePage)
        then:
            tester.assertComponent(HomePage.LATEST_ISSUE_PANEL_ID, NewsletterIssuePanel)
            tester.assertContains("Wydanie #$issue.number")
            tester.assertContains("$issue.heading")
            tester.assertContains("$issue.varia")
    }

    private NewsletterIssueDto prepareExampleIssue() {
        return new NewsletterIssueDto(
                22, LocalDate.now(), "Example heading", "Example varia", Collections.emptyList(), Collections.emptyList()
        )
    }

    def "Should display 'No issue found' when there are no issues"() {
        given:
            newsletterIssueService.getLatestIssue() >> Optional.empty()
        when:
            tester.startPage(HomePage)
        then:
            tester.assertComponent(HomePage.LATEST_ISSUE_PANEL_ID, Label)
            tester.assertContains("Nie znaleziono takiego wydania")
    }



}
