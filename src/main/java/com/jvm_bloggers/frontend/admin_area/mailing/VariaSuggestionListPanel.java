package com.jvm_bloggers.frontend.admin_area.mailing;

import com.jvm_bloggers.domain.query.unread_varia_suggestion.UnreadVariaSuggestion;
import com.jvm_bloggers.frontend.admin_area.panels.CustomFeedbackPanel;
import com.jvm_bloggers.frontend.admin_area.panels.CustomPagingNavigator;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;

import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_FORMATTER;

public class VariaSuggestionListPanel extends Panel {

    public static final String SUGGESTION_DATA_VIEW_ID = "suggestionDataView";
    public static final String ACTIONS_ID = "actions";
    public static final String AUTHOR_ID = "author";
    public static final String CREATE_DATE_ID = "createDate";
    public static final String FORM_ID = "suggestionDataForm";
    public static final String REASON_ID = "reason";
    public static final String URL_ID = "url";

    private final MailingPageBackingBean backingBean;
    private final int defaultPageSize;

    public VariaSuggestionListPanel(String id,
                                    MailingPageBackingBean backingBean,
                                    int defaultPageSize) {
        super(id);
        this.backingBean = backingBean;
        this.defaultPageSize = defaultPageSize;

        CustomFeedbackPanel feedbackPanel = new CustomFeedbackPanel("feedback");
        add(feedbackPanel);
        createDataForm(feedbackPanel);
    }

    private void createDataForm(CustomFeedbackPanel feedbackPanel) {
        Form<Void> form = new Form<>(FORM_ID);
        DataView<UnreadVariaSuggestion> suggestionDataView = createDataView(form, feedbackPanel);
        form.add(suggestionDataView);
        form.add(new CustomPagingNavigator("navigator", suggestionDataView));
        add(form);
    }

    private DataView<UnreadVariaSuggestion> createDataView(Form<Void> form,
                                                           CustomFeedbackPanel feedbackPanel) {
        return new DataView<>(SUGGESTION_DATA_VIEW_ID,
            new VariaSuggestionRequestHandler(backingBean, defaultPageSize)) {
            @Override
            protected void populateItem(Item<UnreadVariaSuggestion> item) {
                UnreadVariaSuggestion unreadVariaSuggestion = item.getModelObject();
                item.add(new Label(AUTHOR_ID, unreadVariaSuggestion.getAuthor()));
                item.add(new Label(REASON_ID, unreadVariaSuggestion.getReason()));
                item.add(new Label(URL_ID, unreadVariaSuggestion.getUrl()));
                item.add(new Label(CREATE_DATE_ID,
                    unreadVariaSuggestion.getCreateDate().format(DATE_FORMATTER)));
                item.add(new VariaSuggestionActionPanel(
                    ACTIONS_ID, backingBean, form, feedbackPanel, unreadVariaSuggestion.getId()));
            }
        };
    }
}
