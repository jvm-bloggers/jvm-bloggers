package com.jvm_bloggers.frontend.public_area.common_layout;

import com.googlecode.wicket.jquery.ui.markup.html.link.BookmarkablePageLink;
import com.jvm_bloggers.domain.query.newsletter_issue_for_listing.NewsletterIssueForListing;
import com.jvm_bloggers.frontend.common_components.NewsletterIssueLink;
import com.jvm_bloggers.frontend.public_area.all_issues.AllIssuesPage;
import com.jvm_bloggers.frontend.public_area.jvm_poland_slack.JvmPolandSlackPage;
import com.jvm_bloggers.frontend.public_area.varia_suggestion.VariaSuggestionPage;

import io.vavr.collection.Seq;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class RightFrontendSidebar extends Panel {

    private static final int NUMBER_OF_LISTED_ISSUES = 5;

    @SpringBean
    private RightFrontendSidebarBackingBean backingBean;

    public RightFrontendSidebar(String id) {
        super(id);
    }

    @Override
    protected void onInitialize() {
        super.onInitialize();
        Seq<NewsletterIssueForListing> latestIssues =
            backingBean.getLatestIssues(NUMBER_OF_LISTED_ISSUES);

        add(new Label("latestIssuesEmptyLabel", "Brak archiwalnych wydań")
                .setVisible(latestIssues.isEmpty())
        );

        ListView<NewsletterIssueForListing> latestIssuesList =
            new ListView<NewsletterIssueForListing>("latestIssuesList", latestIssues.toJavaList()) {
            @Override
            protected void populateItem(ListItem<NewsletterIssueForListing> item) {
                NewsletterIssueForListing issue = item.getModel().getObject();
                item.add(new NewsletterIssueLink(
                    "issueLink",
                    issue.getIssueNumber(),
                    issue.getPublicationDate())
                );
            }
        };
        add(latestIssuesList);
        add(new BookmarkablePageLink<AllIssuesPage>("allIssuesLink", AllIssuesPage.class));
        add(new BookmarkablePageLink<JvmPolandSlackPage>("jvm-poland-slack", JvmPolandSlackPage.class));
        add(new BookmarkablePageLink<VariaSuggestionPage>("variaSuggestionLink",
            VariaSuggestionPage.class));
    }

}
