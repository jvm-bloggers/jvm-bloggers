package pl.tomaszdziurko.jvm_bloggers.view.front_end

import org.apache.wicket.markup.html.basic.Label
import pl.tomaszdziurko.jvm_bloggers.MockSpringContextAwareSpecification
import pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue.NewsletterIssueDto
import pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue.NewsletterIssueDtoService
import pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue.newsletter_panel.NewsletterIssuePanel

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
