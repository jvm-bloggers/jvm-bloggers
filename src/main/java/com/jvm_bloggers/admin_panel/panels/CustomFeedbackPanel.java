package com.jvm_bloggers.admin_panel.panels;

import org.apache.wicket.feedback.FeedbackMessage;
import org.apache.wicket.feedback.IFeedbackMessageFilter;
import org.apache.wicket.markup.html.panel.FeedbackPanel;

public class CustomFeedbackPanel extends FeedbackPanel {

    private static final String COMMON_FEEDBACK_CSS_CLASS = "feedbackPanelItem";

    public CustomFeedbackPanel(String id) {
        super(id);
        setOutputMarkupId(true);
        setOutputMarkupPlaceholderTag(true);
    }

    public CustomFeedbackPanel(String id, IFeedbackMessageFilter filter) {
        super(id, filter);
        setOutputMarkupId(true);
        setOutputMarkupPlaceholderTag(true);
    }

    @Override
    protected String getCSSClass(FeedbackMessage message) {
        return COMMON_FEEDBACK_CSS_CLASS + " " + super.getCSSClass(message);
    }
}
