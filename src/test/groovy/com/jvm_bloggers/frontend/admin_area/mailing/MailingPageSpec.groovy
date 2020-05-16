package com.jvm_bloggers.frontend.admin_area.mailing

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.core.mailing.BlogSummaryMailGenerator
import com.jvm_bloggers.core.mailing.IssueNumberRetriever
import com.jvm_bloggers.core.newsletter_issues.NewsletterIssueFactory
import com.jvm_bloggers.entities.metadata.Metadata
import com.jvm_bloggers.entities.metadata.MetadataKeys
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration
import com.jvm_bloggers.utils.ZoneTimeProvider
import org.apache.wicket.Page
import org.apache.wicket.util.tester.FormTester

import static com.jvm_bloggers.entities.metadata.MetadataKeys.MAILING_GREETING
import static com.jvm_bloggers.entities.metadata.MetadataKeys.MAILING_TEMPLATE
import static com.jvm_bloggers.frontend.admin_area.mailing.MailingPage.*

class MailingPageSpec extends MockSpringContextAwareSpecification {

    static final String MAILING_TEMPLATE_VALUE = "Example Mailing Template"
    static final String GREETING_VALUE = "Hello all"
    static final String DEFAULT_MAILING_TEMPLATE_VALUE = "Default Mailing Template"
    static final int HEADING_TEMPLATE_IDX = 2

    BlogSummaryMailGenerator blogSummaryMailGenerator = Stub(BlogSummaryMailGenerator)
    IssueNumberRetriever issueNumberRetriever = Stub(IssueNumberRetriever)
    MailingPageBackingBean backingBean = Mock(MailingPageBackingBean)
    NewsletterIssueFactory newsletterIssueFactory = Stub(NewsletterIssueFactory)

    @Override
    void setupContext() {
        addBean(backingBean)
        addBean(blogSummaryMailGenerator)
        addBean(issueNumberRetriever)
        addBean(newsletterIssueFactory)
        addBean(new ZoneTimeProvider())
        addBean(new PaginationConfiguration(15))

        backingBean.findMetadataByName(MAILING_TEMPLATE) >> new Metadata(
            0L,
            MAILING_TEMPLATE,
            MAILING_TEMPLATE_VALUE
        )

        backingBean.findMetadataByName(MAILING_GREETING) >> new Metadata(
            1L,
            MAILING_GREETING,
            GREETING_VALUE
        )
    }

    def "Should display value of Mailing Template in wysiwyg editor"() {
        when:
        tester.startPage(MailingPage.class)

        then:
        Page currentPage = tester.getLastRenderedPage()
        String wysiwygModelValue = currentPage.get(MAILING_TEMPLATE_FORM_ID + ":" + WYSIWYG_ID).getDefaultModelObjectAsString()
        wysiwygModelValue == MAILING_TEMPLATE_VALUE
    }

    def "Should restore default value of MailingTemplate after reset button is clicked"() {
        given:
        backingBean.findMetadataByName(MetadataKeys.DEFAULT_MAILING_TEMPLATE) >> new Metadata(
            0L,
            MetadataKeys.DEFAULT_MAILING_TEMPLATE,
            DEFAULT_MAILING_TEMPLATE_VALUE
        )
        tester.startPage(MailingPage.class)

        when:
        FormTester formTester = tester.newFormTester(MAILING_TEMPLATE_FORM_ID)
        formTester.submit(RESET_MAILING_TEMPLATE_BUTTON_ID)

        then:
        1 * backingBean.saveMetadata({ it.name == MAILING_TEMPLATE && it.value == DEFAULT_MAILING_TEMPLATE_VALUE })
    }

    def "Should save MailingTemplate with empty value"() {
        given:
            backingBean.findMetadataByName(MetadataKeys.HEADING_TEMPLATE) >> new Metadata(
                    0L,
                    MetadataKeys.HEADING_TEMPLATE,
                    ""
            )
            tester.startPage(MailingPage.class)

        when:
            FormTester formTester = tester.newFormTester(MAILING_TEMPLATE_FORM_ID)
            formTester.select(MAILING_SECTION_TO_EDIT_DROPDOWN_ID, HEADING_TEMPLATE_IDX)
            tester.executeAjaxEvent(MAILING_TEMPLATE_FORM_ID + ":" + MAILING_SECTION_TO_EDIT_DROPDOWN_ID, "change")
            formTester.submit(SAVE_BUTTON_ID)

        then:
            1 * backingBean.saveMetadata({ it.name == MetadataKeys.HEADING_TEMPLATE && it.value == "" })
    }

    def "Should send test email when button clicked"() {
        given:
        tester.startPage(MailingPage.class)

        when:
        FormTester formTester = tester.newFormTester(MAILING_TEMPLATE_FORM_ID)
        formTester.submit(SEND_TEST_MAIL_BUTTON_ID)

        then:
        1 * backingBean.sendTestEmail()
    }

    def "Should change model in wysiwyg editor when dropdown value is changed to greeting section"() {
        given:
        tester.startPage(MailingPage.class)

        when:
        FormTester formTester = tester.newFormTester(MAILING_TEMPLATE_FORM_ID)
        formTester.select(MAILING_SECTION_TO_EDIT_DROPDOWN_ID, 1)
        tester.executeAjaxEvent(MAILING_TEMPLATE_FORM_ID + ":" + MAILING_SECTION_TO_EDIT_DROPDOWN_ID, "change")

        then:
        Page currentPage = tester.getLastRenderedPage()
        String wysiwygModelValue = currentPage.get(MAILING_TEMPLATE_FORM_ID + ":" + WYSIWYG_ID).getDefaultModelObjectAsString()
        wysiwygModelValue == GREETING_VALUE
    }

    def "Should display varia suggestion panel when dropdown value is set to viara section"() {
        given:
        tester.startPage(MailingPage)
        boolean initVariaSuggestion = tester.getLastRenderedPage().get(VARIA_SUGGESTION_PANEL_ID).isVisible()

        when:
        FormTester formTester = tester.newFormTester(MAILING_TEMPLATE_FORM_ID)
        formTester.select(MAILING_SECTION_TO_EDIT_DROPDOWN_ID, 3)
        tester.executeAjaxEvent(MAILING_TEMPLATE_FORM_ID + ":" + MAILING_SECTION_TO_EDIT_DROPDOWN_ID, "change")

        then:
        Page currentPage = tester.getLastRenderedPage()
        currentPage.get(VARIA_SUGGESTION_PANEL_ID).isVisible()

        and:
        !initVariaSuggestion
    }
}
