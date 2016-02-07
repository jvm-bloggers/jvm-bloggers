package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import pl.tomaszdziurko.jvm_bloggers.mailing.BlogSummaryMailGenerator;
import pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

import java.time.LocalDateTime;

public class MailingTemplatePreviewPanel extends Panel {

    @SpringBean
    private BlogSummaryMailGenerator blogSummaryMailGenerator;

    @SpringBean
    private NowProvider nowProvider;

    public MailingTemplatePreviewPanel(String id) {
        super(id);
        Injector.get().inject(this);

        Label templatePreview = new Label("templatePreview", new LoadableDetachableModel<String>() {
            @Override
            protected String load() {
                LocalDateTime now = nowProvider.now();
                int daysSinceLastFriday = DateTimeUtilities.daysBetweenDateAndLastFriday(now);
                String mailContent = blogSummaryMailGenerator.prepareMailContent(daysSinceLastFriday);
                return mailContent;
            }
        });
        templatePreview.setEscapeModelStrings(false);
        add(templatePreview);
    }

}
