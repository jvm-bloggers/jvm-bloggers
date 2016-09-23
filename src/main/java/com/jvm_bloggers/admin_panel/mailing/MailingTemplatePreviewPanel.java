package com.jvm_bloggers.admin_panel.mailing;

import com.jvm_bloggers.core.mailing.BlogSummaryMailGenerator;
import com.jvm_bloggers.core.mailing.IssueNumberRetriever;
import com.jvm_bloggers.core.newsletter_issues.NewsletterIssueFactory;
import com.jvm_bloggers.core.newsletter_issues.domain.NewsletterIssue;
import com.jvm_bloggers.utils.DateTimeUtilities;
import com.jvm_bloggers.utils.NowProvider;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.time.LocalDateTime;

public class MailingTemplatePreviewPanel extends Panel {

    @SpringBean
    private NewsletterIssueFactory newsletterIssueFactory;

    @SpringBean
    private BlogSummaryMailGenerator blogSummaryMailGenerator;

    @SpringBean
    private IssueNumberRetriever issueNumberRetriever;

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
                NewsletterIssue newsletterIssue = newsletterIssueFactory.create(
                    daysSinceLastFriday,
                    issueNumberRetriever.getCurrentIssueNumber() + 1
                );
                return blogSummaryMailGenerator.prepareMailContent(newsletterIssue);
            }
        });
        templatePreview.setEscapeModelStrings(false);
        add(templatePreview);
    }

}
