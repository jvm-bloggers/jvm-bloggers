package com.jvm_bloggers.frontend.public_area.varia_suggestion

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebarBackingBean
import org.apache.wicket.ajax.markup.html.form.AjaxButton
import org.apache.wicket.markup.html.form.Form
import org.apache.wicket.markup.html.form.TextArea
import org.apache.wicket.markup.html.form.TextField
import org.apache.wicket.util.tester.FormTester

import static com.jvm_bloggers.frontend.WicketTestUtils.pathVia
import static com.jvm_bloggers.frontend.public_area.varia_suggestion.VariaSuggestionPage.AUTHOR_ID
import static com.jvm_bloggers.frontend.public_area.varia_suggestion.VariaSuggestionPage.FORM_ID
import static com.jvm_bloggers.frontend.public_area.varia_suggestion.VariaSuggestionPage.REASON_ID
import static com.jvm_bloggers.frontend.public_area.varia_suggestion.VariaSuggestionPage.SUBMIT_ID
import static com.jvm_bloggers.frontend.public_area.varia_suggestion.VariaSuggestionPage.URL_ID

class VariaSuggestionPageSpec extends MockSpringContextAwareSpecification {

    VariaSuggestionPageBackingBean backingBean = Mock(VariaSuggestionPageBackingBean)

    @Override
    protected void setupContext() {
        addBean(backingBean)
        addBean(Stub(RightFrontendSidebarBackingBean))
    }

    def "Should render suggestion form"() {
        when:
        tester.startPage(VariaSuggestionPage)

        then:
        tester.assertComponent(FORM_ID, Form)
        tester.assertComponent(pathVia(FORM_ID, URL_ID), TextField)
        tester.assertComponent(pathVia(FORM_ID, AUTHOR_ID), TextField)
        tester.assertComponent(pathVia(FORM_ID, REASON_ID), TextArea)
        tester.assertComponent(pathVia(FORM_ID, SUBMIT_ID), AjaxButton)
    }

    def "Should load empty model"() {
        when:
        tester.startPage(VariaSuggestionPage)
        VariaSuggestionModel model = tester.getComponentFromLastRenderedPage(FORM_ID)
                .getDefaultModelObject() as VariaSuggestionModel

        then:
        model.url == null
        model.author == null
        model.reason == null
    }

    def "Should clear form after submit"() {
        when:
        tester.startPage(VariaSuggestionPage)
        FormTester formTester = tester.newFormTester(FORM_ID)
        formTester.setValue(URL_ID, 'https://jvm-bloggers.com')
        formTester.setValue(AUTHOR_ID, 'author')
        formTester.setValue(REASON_ID, 'reason')
        formTester.submit(SUBMIT_ID)

        VariaSuggestionModel model = tester.getComponentFromLastRenderedPage(FORM_ID)
                .getDefaultModelObject() as VariaSuggestionModel

        then:
        model.url == null
        model.author == null
        model.reason == null
    }

    def "Should create suggestion from valid form"() {
        given:
        VariaSuggestionModel model = new VariaSuggestionModel()
        model.url = 'https://jvm-bloggers.com'
        model.author = 'author'
        model.reason = 'reason'

        when:
        tester.startPage(VariaSuggestionPage)
        FormTester formTester = tester.newFormTester(FORM_ID)
        formTester.setValue(URL_ID, model.url)
        formTester.setValue(AUTHOR_ID, model.author)
        formTester.setValue(REASON_ID, model.reason)
        formTester.submit(SUBMIT_ID)

        then:
        1 * backingBean.createVariaSuggestion({
            it.url == model.url &&
            it.author == model.author &&
            it.reason == model.reason
        })
    }

    def "Should create suggestion with uppercase url"() {
        given:
        VariaSuggestionModel model = new VariaSuggestionModel()
        model.url = 'HTTPS://JVM-BLOGGERS.COM'
        model.author = 'author'
        model.reason = 'reason'

        when:
        tester.startPage(VariaSuggestionPage)
        FormTester formTester = tester.newFormTester(FORM_ID)
        formTester.setValue(URL_ID, model.url)
        formTester.setValue(AUTHOR_ID, model.author)
        formTester.setValue(REASON_ID, model.reason)
        formTester.submit(SUBMIT_ID)

        then:
        1 * backingBean.createVariaSuggestion({
            it.url == model.url &&
            it.author == model.author &&
            it.reason == model.reason
        })
    }


    def "Should not create suggestion from invalid form"() {
        when:
        tester.startPage(VariaSuggestionPage)
        FormTester formTester = tester.newFormTester(FORM_ID)
        formTester.submit(SUBMIT_ID)

        then:
        0 * backingBean.createVariaSuggestion(_)
    }
}
