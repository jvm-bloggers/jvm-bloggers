package com.jvm_bloggers.frontend.public_area

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.domain.query.newsletter_issue_for_listing.NewsletterIssueForListing
import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedNewsletterIssue
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebarBackingBean
import com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel.NewsletterIssuePanel
import io.vavr.collection.List
import io.vavr.control.Option
import org.apache.wicket.markup.html.basic.Label

import java.time.LocalDate
import java.util.function.Consumer

import static com.jvm_bloggers.domain.query.NewsletterIssueNumber.of
import static com.jvm_bloggers.frontend.public_area.HomePage.LATEST_ISSUE_PANEL_ID
import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_FORMATTER
import static io.vavr.collection.List.empty
import static io.vavr.control.Option.none
import static java.time.LocalDate.now

class HomePageSpec extends MockSpringContextAwareSpecification {

    HomePageBackingBean backingBean = Stub(HomePageBackingBean)
    RightFrontendSidebarBackingBean sidebarBean = Stub(RightFrontendSidebarBackingBean)

    @Override
    protected void setupContext() {
        addBean(backingBean)
        addBean(sidebarBean)
    }

    def "Should display latest issue"() {
        given:
        PublishedNewsletterIssue issue = prepareExampleIssue()
        backingBean.getLatestIssue() >> Option.of(issue)

        when:
        tester.startPage(HomePage)

        then:
        tester.assertComponent(LATEST_ISSUE_PANEL_ID, NewsletterIssuePanel)
        tester.assertContains("Wydanie #${issue.getNumber().asLong()}")
        tester.assertContains("$issue.headingSection()")
        tester.assertContains("$issue.variaSection")
    }

    private PublishedNewsletterIssue prepareExampleIssue() {
        return new PublishedNewsletterIssue(
                of(22L),
                now(),
                "Example heading",
                "Example varia",
                empty(),
                empty(),
                empty(),
                empty(),
                empty()
        )
    }

    def "Should display 'No issue found' when there are no issues"() {
        given:
        mockEmptyLatestIssue()

        when:
        tester.startPage(HomePage)

        then:
        tester.assertComponent(LATEST_ISSUE_PANEL_ID, Label)
        tester.assertContains("Nie istnieje żadne wydanie newslettera.")
    }

    private void mockEmptyLatestIssue() {
        backingBean.getLatestIssue() >> none()
    }

    def "Should list last 5 newsletter issues on right panel"() {
        given:
        mockEmptyLatestIssue()
        List<NewsletterIssueForListing> latestIssues = List.of(
            createIssueForListing(5),
            createIssueForListing(4),
            createIssueForListing(3),
            createIssueForListing(2),
            createIssueForListing(1)
        )
        sidebarBean.getLatestIssues(_) >> latestIssues

        when:
        tester.startPage(HomePage)

        then:
        latestIssues.forEach(new Consumer<NewsletterIssueForListing>() {
            @Override
            void accept(NewsletterIssueForListing issue) {
                int issueNumber = issue.getIssueNumber().asLong()
                String publicationDateAsString = DATE_FORMATTER.format(issue.getPublicationDate())
                tester.assertContains("Wydanie #$issueNumber - $publicationDateAsString")
            }
        })
    }

    private NewsletterIssueForListing createIssueForListing(int i) {
        return new NewsletterIssueForListing(of(i), LocalDate.now())
    }

    def "Should show appropriate message on right panel, when no newsletters"() {
        given:
        mockEmptyLatestIssue()
        sidebarBean.getLatestIssues(_) >> empty()

        when:
        tester.startPage(HomePage)

        then:
        tester.assertContains("Brak archiwalnych wydań")
    }

}
