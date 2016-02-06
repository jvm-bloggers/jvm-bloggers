package pl.tomaszdziurko.jvm_bloggers.view.panels;

import org.apache.wicket.feedback.FeedbackMessage;
import org.apache.wicket.feedback.IFeedbackMessageFilter;
import org.apache.wicket.markup.html.panel.FeedbackPanel;

public class CustomFeedbackPanel extends FeedbackPanel {

    final static String COMMON_FEEDBACK_CSS_CLASS = "feedbackPanelItem";

    public CustomFeedbackPanel(String id) {
        super(id);
    }

    public CustomFeedbackPanel(String id, IFeedbackMessageFilter filter) {
        super(id, filter);
    }


    @Override
    protected String getCSSClass(FeedbackMessage message) {
        return COMMON_FEEDBACK_CSS_CLASS + " " + super.getCSSClass(message);
    }
}
