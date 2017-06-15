package com.jvm_bloggers.frontend.public_area.newsletter_issue

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.domain.query.NewsletterIssueNumber
import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedNewsletterIssue
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebarBackingBean
import com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel.NewsletterIssueNavigationLink
import com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel.NewsletterIssuePanel
import javaslang.control.Option
import org.apache.wicket.markup.html.basic.Label
import org.springframework.web.util.HtmlUtils
import spock.lang.Subject

import static com.jvm_bloggers.domain.query.NewsletterIssueNumber.of
import static com.jvm_bloggers.frontend.public_area.newsletter_issue.NewsletterIssuePage.ISSUE_PANEL_ID
import static com.jvm_bloggers.frontend.public_area.newsletter_issue.NewsletterIssuePage.buildShowIssueParams
import static javaslang.collection.List.empty
import static java.lang.String.format
import static java.time.LocalDate.now

class NewsletterIssuePageSpec extends MockSpringContextAwareSpecification {

    NewsletterIssuePageBackingBean backingBean = Stub(NewsletterIssuePageBackingBean)
    RightFrontendSidebarBackingBean sidebarBackingBean = Stub(RightFrontendSidebarBackingBean)

    @Override
    protected void setupContext() {
        addBean(backingBean)
        addBean(sidebarBackingBean)
    }

    def "Should display selected issue"() {
        given:
        PublishedNewsletterIssue issue = prepareExampleIssue()
        backingBean.findByIssueNumber(issue.number) >> Option.of(issue)

        when:
        tester.startPage(NewsletterIssuePage, NewsletterIssuePage.buildShowIssueParams(issue.number))

        then:
        tester.assertComponent(ISSUE_PANEL_ID, NewsletterIssuePanel)
        tester.assertContains("Wydanie #${issue.number.asLong()}")
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
                empty()
        )
    }

    def "Should display 'No issue found' when there is issue with a given number"() {
        given:
        NewsletterIssueNumber issueNumber = of(34)
        backingBean.findByIssueNumber(issueNumber) >> Option.none()

        when:
        tester.startPage(NewsletterIssuePage, NewsletterIssuePage.buildShowIssueParams(issueNumber))

        then:
        tester.assertComponent(ISSUE_PANEL_ID, Label)
        tester.assertContains("Nie znaleziono takiego wydania")
    }

    def "Should display previous navigation link"() {
        given:
        PublishedNewsletterIssue issue = prepareExampleIssue()
        NewsletterIssueNumber previous = NewsletterIssueNumber.previous(issue.number)

        @Subject
        NewsletterIssuePanel newsletterIssuePanel = new NewsletterIssuePanel(
                ISSUE_PANEL_ID,
                issue,
                Option.none(),
                Option.of(previous))

        when:
        tester.startComponentInPage(newsletterIssuePanel)

        then:
        tester.assertComponent("$ISSUE_PANEL_ID:previousNewsletterIssueNumber", NewsletterIssueNavigationLink)
        tester.assertBookmarkablePageLink("$ISSUE_PANEL_ID:previousNewsletterIssueNumber", NewsletterIssuePage, buildShowIssueParams(previous))
        tester.getTagByWicketId('previousNewsletterIssueNumber').value == HtmlUtils.htmlEscape(format(NewsletterIssueNavigationLink.Direction.PRESIOUS.value, previous.asLong()))
    }

    def "Should display next navigation link"() {
        given:
        PublishedNewsletterIssue issue = prepareExampleIssue()
        NewsletterIssueNumber next = NewsletterIssueNumber.next(issue.number)

        @Subject
        NewsletterIssuePanel newsletterIssuePanel = new NewsletterIssuePanel(
                ISSUE_PANEL_ID,
                issue,
                Option.of(next),
                Option.none())

        when:
        tester.startComponentInPage(newsletterIssuePanel)

        then:
        tester.assertComponent("$ISSUE_PANEL_ID:nextNewsletterIssueNumber", NewsletterIssueNavigationLink)
        tester.assertBookmarkablePageLink("$ISSUE_PANEL_ID:nextNewsletterIssueNumber", NewsletterIssuePage, buildShowIssueParams(next))
        tester.getTagByWicketId('nextNewsletterIssueNumber').value == HtmlUtils.htmlEscape(format(NewsletterIssueNavigationLink.Direction.NEXT.value, next.asLong()))
    }

    def "Should not display previous navigation link"() {
        given:
        PublishedNewsletterIssue issue = prepareExampleIssue()

        @Subject
        NewsletterIssuePanel newsletterIssuePanel = new NewsletterIssuePanel(
                ISSUE_PANEL_ID,
                issue,
                Option.none(),
                Option.none())

        when:
        tester.startComponentInPage(newsletterIssuePanel)

        then:
        tester.getTagByWicketId('previousNewsletterIssueNumber') == null
    }

    def "Should not display next navigation links"() {
        given:
        PublishedNewsletterIssue issue = prepareExampleIssue()

        @Subject
        NewsletterIssuePanel newsletterIssuePanel = new NewsletterIssuePanel(
                ISSUE_PANEL_ID,
                issue,
                Option.none(),
                Option.none())

        when:
        tester.startComponentInPage(newsletterIssuePanel)

        then:
        tester.getTagByWicketId('nextNewsletterIssueNumber') == null
    }

}
