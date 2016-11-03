package com.jvm_bloggers.frontend.common_layout;

import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssueDto;
import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssueDtoService;
import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssuePage;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.StringResourceModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author rafal.garbowski
 */
public class RightFrontendSidebar extends Panel {

    static final DateTimeFormatter PUBLISHED_DATE_FORMATTER = DateTimeFormatter.ISO_DATE;

    @SpringBean
    private NewsletterIssueDtoService newsletterIssueDtoService;

    public RightFrontendSidebar(String id) {
        super(id);
        composeLatestFiveNewsletterIssuesLinksView();
    }

    private void composeLatestFiveNewsletterIssuesLinksView() {
        List<Link> latestIssues = newsletterIssueDtoService
            .findTop5ByOrderByPublishedDateDesc().stream().map(this::getLink)
            .collect(Collectors.toList());

        if (latestIssues.isEmpty()) {
            add(new Label("latestIssuesEmptyLabel", getString("right.panel.latest.issues.empty")));
        } else {
            add(new EmptyPanel("latestIssuesEmptyLabel"));
        }

        add(new ListView<Link>("latestIssuesList", latestIssues) {
            @Override
            protected void populateItem(ListItem<Link> item) {
                item.add(item.getModelObject());
            }
        });
    }

    private Link getLink(NewsletterIssueDto issue) {
        return (Link) new BookmarkablePageLink<>("issueLink", NewsletterIssuePage.class,
            NewsletterIssuePage.buildShowIssueParams(issue.number))
            .setBody(Model.of(new StringResourceModel("right.panel.issue.link.label")
                .setParameters(issue.number,
                    PUBLISHED_DATE_FORMATTER.format(issue.publishedDate))));
    }
}
