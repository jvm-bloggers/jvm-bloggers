package com.jvm_bloggers.frontend

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.frontend.public_area.HomePage
import com.jvm_bloggers.frontend.public_area.newsletter_issue.NewsletterIssueDto
import com.jvm_bloggers.frontend.public_area.newsletter_issue.NewsletterIssueDtoService
import com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel.NewsletterIssuePanel
import com.jvm_bloggers.utils.DateTimeUtilities
import org.apache.wicket.markup.html.basic.Label

import java.text.NumberFormat
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
            mockEmptyLatestIssue()
        when:
            tester.startPage(HomePage)
        then:
            tester.assertComponent(HomePage.LATEST_ISSUE_PANEL_ID, Label)
            tester.assertContains("Nie znaleziono takiego wydania")
    }

    private void mockEmptyLatestIssue() {
        newsletterIssueService.getLatestIssue() >> Optional.empty()
    }

    def "Should list last 5 newsletter issues on right panel"() {
        given:
            mockEmptyLatestIssue()
            List<NewsletterIssueDto> latestIssues = []
            (1..5).each {
                latestIssues.add(NewsletterIssueDto.builder().number(5_000 + it).publishedDate(LocalDate.of(2015, 5, it)).build())
            }
            newsletterIssueService.findTop5ByOrderByPublishedDateDesc() >> latestIssues
        when:
            tester.startPage(HomePage)
        then:
            latestIssues.each {
                String issueNumber = NumberFormat.getInstance().format(it.number)
                String publishedDate = DateTimeUtilities.DATE_FORMATTER.format(it.publishedDate)
                tester.assertContains("Wydanie #$issueNumber - $publishedDate")
            }
    }

    def "Should show appropriate message on right panel, when no newsletters"() {
        given:
            mockEmptyLatestIssue()
            newsletterIssueService.findTop5ByOrderByPublishedDateDesc() >> []
        when:
            tester.startPage(HomePage)
        then:
            tester.assertContains("Newsletterów brak")
    }
}
