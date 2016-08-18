package pl.tomaszdziurko.jvm_bloggers.view.mailing

import org.apache.wicket.Page
import org.apache.wicket.util.tester.FormTester
import pl.tomaszdziurko.jvm_bloggers.MockSpringContextAwareSpecification
import pl.tomaszdziurko.jvm_bloggers.mailing.BlogSummaryMailGenerator
import pl.tomaszdziurko.jvm_bloggers.mailing.IssueNumberRetriever
import pl.tomaszdziurko.jvm_bloggers.metadata.Metadata
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataKeys
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataRepository
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.NewsletterIssueFactory
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider
import pl.tomaszdziurko.jvm_bloggers.view.admin.PaginationConfiguration
import pl.tomaszdziurko.jvm_bloggers.view.admin.mailing.MailingPage
import pl.tomaszdziurko.jvm_bloggers.view.admin.mailing.MailingPageRequestHandler

import static pl.tomaszdziurko.jvm_bloggers.metadata.MetadataKeys.MAILING_GREETING
import static pl.tomaszdziurko.jvm_bloggers.metadata.MetadataKeys.MAILING_TEMPLATE
import static pl.tomaszdziurko.jvm_bloggers.view.admin.mailing.MailingPage.*

class MailingPageSpec extends MockSpringContextAwareSpecification {

    static final String MAILING_TEMPLATE_VALUE = "Example Mailing Template"
    static final String GREETING_VALUE = "Hello all"
    static final String DEFAULT_MAILING_TEMPLATE_VALUE = "Default Mailing Template"

    MailingPageRequestHandler mailingPageRequestHandler = Mock(MailingPageRequestHandler)
    MetadataRepository metadataRepository = Mock(MetadataRepository)
    NewsletterIssueFactory newsletterIssueFactory = Stub(NewsletterIssueFactory)
    BlogSummaryMailGenerator blogSummaryMailGenerator = Stub(BlogSummaryMailGenerator)
    IssueNumberRetriever issueNumberRetriever = Stub(IssueNumberRetriever)

    def void setupContext() {
        NowProvider nowProvider = new NowProvider()
        addBean(nowProvider)
        addBean(mailingPageRequestHandler)
        addBean(metadataRepository)
        addBean(newsletterIssueFactory)
        addBean(blogSummaryMailGenerator)
        addBean(issueNumberRetriever)
        addBean(new PaginationConfiguration(15))

        metadataRepository.findByName(MAILING_TEMPLATE) >> new Metadata(
                0L,
                MAILING_TEMPLATE,
                MAILING_TEMPLATE_VALUE
        )

        metadataRepository.findByName(MAILING_GREETING) >> new Metadata(
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
            metadataRepository.findByName(MetadataKeys.DEFAULT_MAILING_TEMPLATE) >> new Metadata(
                    0L,
                    MetadataKeys.DEFAULT_MAILING_TEMPLATE,
                    DEFAULT_MAILING_TEMPLATE_VALUE
            )
            tester.startPage(MailingPage.class)
        when:
            FormTester formTester = tester.newFormTester(MAILING_TEMPLATE_FORM_ID)
            formTester.submit(RESET_MAILING_TEMPLATE_BUTTON_ID)
        then:
            1 * metadataRepository.save( {it.name == MAILING_TEMPLATE && it.value == DEFAULT_MAILING_TEMPLATE_VALUE})
    }

    def "Should send test email when button clicked"() {
        given:
            tester.startPage(MailingPage.class)
        when:
            FormTester formTester = tester.newFormTester(MAILING_TEMPLATE_FORM_ID)
            formTester.submit(SEND_TEST_MAIL_BUTTON_ID)
        then:
            1 * mailingPageRequestHandler.sendTestEmail()
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

}
