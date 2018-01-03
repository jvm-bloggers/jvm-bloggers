package com.jvm_bloggers.frontend.admin_area.mailing;

import com.jvm_bloggers.frontend.admin_area.panels.CustomFeedbackPanel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;

public class VariaSuggestionActionPanel extends Panel {

    public static final String BUTTON_ID = "markRead";

    private final MailingPageBackingBean backingBean;
    private final Form<?> form;
    private final CustomFeedbackPanel feedbackPanel;
    private final long variaSuggestionId;

    public VariaSuggestionActionPanel(String id,
                                      MailingPageBackingBean backingBean,
                                      Form<?> form,
                                      CustomFeedbackPanel feedbackPanel,
                                      long variaSuggestionId) {
        super(id);
        this.backingBean = backingBean;
        this.form = form;
        this.feedbackPanel = feedbackPanel;
        this.variaSuggestionId = variaSuggestionId;

        createMarkReadButton();
    }

    private void createMarkReadButton() {
        AjaxButton button = new AjaxButton(BUTTON_ID, form) {
            @Override
            protected void onSubmit(AjaxRequestTarget target) {
                backingBean.markVariaSuggestionAsRead(variaSuggestionId);
                getSession().success("Marked as read");
                target.add(getForm());
                target.add(feedbackPanel);
            }
        };
        add(button);
    }
}
