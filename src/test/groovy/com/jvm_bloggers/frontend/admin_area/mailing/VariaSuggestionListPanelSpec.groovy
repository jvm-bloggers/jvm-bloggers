package com.jvm_bloggers.frontend.admin_area.mailing

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.domain.query.unread_varia_suggestion.UnreadVariaSuggestion
import io.vavr.collection.List
import org.apache.wicket.markup.html.basic.Label
import org.apache.wicket.markup.repeater.data.DataView

import java.time.LocalDateTime

import static com.jvm_bloggers.frontend.WicketTestUtils.pathVia
import static com.jvm_bloggers.frontend.admin_area.mailing.MailingPage.VARIA_SUGGESTION_PANEL_ID
import static com.jvm_bloggers.frontend.admin_area.mailing.VariaSuggestionActionPanel.BUTTON_ID
import static com.jvm_bloggers.frontend.admin_area.mailing.VariaSuggestionListPanel.ACTIONS_ID
import static com.jvm_bloggers.frontend.admin_area.mailing.VariaSuggestionListPanel.AUTHOR_ID
import static com.jvm_bloggers.frontend.admin_area.mailing.VariaSuggestionListPanel.CREATE_DATE_ID
import static com.jvm_bloggers.frontend.admin_area.mailing.VariaSuggestionListPanel.FORM_ID
import static com.jvm_bloggers.frontend.admin_area.mailing.VariaSuggestionListPanel.REASON_ID
import static com.jvm_bloggers.frontend.admin_area.mailing.VariaSuggestionListPanel.SUGGESTION_DATA_VIEW_ID
import static com.jvm_bloggers.frontend.admin_area.mailing.VariaSuggestionListPanel.URL_ID

class VariaSuggestionListPanelSpec extends MockSpringContextAwareSpecification {

    MailingPageBackingBean backingBean = Mock()

    def "Should render varia suggestion list"() {
        given:
        backingBean.countUnreadVariaSuggestion() >> 4L
        backingBean.findUnreadSuggestions(_, _) >> List.of(
                new UnreadVariaSuggestion(1L, 'url', 'author', 'reason', LocalDateTime.now()),
                new UnreadVariaSuggestion(2L, 'url', 'author', 'reason', LocalDateTime.now()),
                new UnreadVariaSuggestion(3L, 'url', 'author', 'reason', LocalDateTime.now()),
                new UnreadVariaSuggestion(4L, 'url', 'author', 'reason', LocalDateTime.now())
        )

        when:
        tester.startComponentInPage(new VariaSuggestionListPanel(
                VARIA_SUGGESTION_PANEL_ID,
                backingBean,
                10))

        then:
        DataView dv = tester.getComponentFromLastRenderedPage(
                pathVia(VARIA_SUGGESTION_PANEL_ID, FORM_ID, SUGGESTION_DATA_VIEW_ID)) as DataView
        dv.size() == 4
        tester.assertComponent(pathVia(VARIA_SUGGESTION_PANEL_ID, FORM_ID, SUGGESTION_DATA_VIEW_ID, '1', AUTHOR_ID), Label)
        tester.assertComponent(pathVia(VARIA_SUGGESTION_PANEL_ID, FORM_ID, SUGGESTION_DATA_VIEW_ID, '1', REASON_ID), Label)
        tester.assertComponent(pathVia(VARIA_SUGGESTION_PANEL_ID, FORM_ID, SUGGESTION_DATA_VIEW_ID, '1', URL_ID), Label)
        tester.assertComponent(pathVia(VARIA_SUGGESTION_PANEL_ID, FORM_ID, SUGGESTION_DATA_VIEW_ID, '1', CREATE_DATE_ID), Label)
    }

    def "Should mark varia suggestion as read"() {
        given:
        backingBean.countUnreadVariaSuggestion() >> 1L
        backingBean.findUnreadSuggestions(_, _) >> List.of(
                new UnreadVariaSuggestion(1L, 'url', 'author', 'reason', LocalDateTime.now()),
        )

        when:
        tester.startComponentInPage(new VariaSuggestionListPanel(
                VARIA_SUGGESTION_PANEL_ID,
                backingBean,
                10))
        tester.executeAjaxEvent(pathVia(VARIA_SUGGESTION_PANEL_ID, FORM_ID, SUGGESTION_DATA_VIEW_ID, '1', ACTIONS_ID, BUTTON_ID), 'click')

        then:
        1 * backingBean.markVariaSuggestionAsRead({ it == 1L})
    }

    @Override
    protected void setupContext() {

    }
}
