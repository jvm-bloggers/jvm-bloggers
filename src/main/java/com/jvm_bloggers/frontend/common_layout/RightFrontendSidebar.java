package com.jvm_bloggers.frontend.common_layout;

import com.googlecode.wicket.jquery.ui.markup.html.link.BookmarkablePageLink;
import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssueDto;
import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssueDtoService;
import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssuePage;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.AbstractLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.StringResourceModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.springframework.context.i18n.LocaleContextHolder;

import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author rafal.garbowski
 */
public class RightFrontendSidebar extends Panel {

    public static final String PUBLISHED_DATE_FORMAT = "d MMMM";

    @SpringBean
    private NewsletterIssueDtoService newsletterIssueDtoService;

    public RightFrontendSidebar(String id) {
        super(id);
        composeLatestFiveNewsletterIssuesLinksView();
    }

    private void composeLatestFiveNewsletterIssuesLinksView() {
        List<AbstractLink> latestIssues = newsletterIssueDtoService
            .findTop5ByOrderByPublishedDateDesc().stream().map(this::getLink)
            .collect(Collectors.toList());

        if (latestIssues.isEmpty()) {
            add(new Label("latestIssuesEmptyLabel", getString("right.panel.latest.issues.empty")));
        } else {
            add(new EmptyPanel("latestIssuesEmptyLabel"));
        }

        add(new ListView<AbstractLink>("latestIssuesList", latestIssues) {
            @Override
            protected void populateItem(ListItem<AbstractLink> item) {
                item.add(item.getModelObject());
            }
        });
    }

    private AbstractLink getLink(NewsletterIssueDto issue) {
        return new BookmarkablePageLink<>("issueLink", NewsletterIssuePage.class,
            NewsletterIssuePage.buildShowIssueParams(issue.number))
            .setBody(Model.of(new StringResourceModel("right.panel.issue.link.label")
                .setParameters(issue.number,
                    DateTimeFormatter
                        .ofPattern(PUBLISHED_DATE_FORMAT, LocaleContextHolder.getLocale())
                        .format(issue.publishedDate))));
    }
}
