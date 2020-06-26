package com.jvm_bloggers.frontend.public_area.all_issues;

import com.jvm_bloggers.domain.query.newsletter_issue_for_listing.NewsletterIssueForListing;
import com.jvm_bloggers.frontend.common_components.NewsletterIssueLink;

import io.vavr.collection.Seq;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;

import java.time.YearMonth;

import static com.jvm_bloggers.utils.DateTimeUtilities.stringify;

class IssuesInMonthPanel extends Panel {

    IssuesInMonthPanel(String id, YearMonth yearMonth,
                       Seq<NewsletterIssueForListing> monthIssues) {
        super(id);
        add(new Label("groupLabel", stringify(yearMonth)));
        add(new ListView<>("issuesList", monthIssues.toJavaList()) {
            @Override
            protected void populateItem(ListItem<NewsletterIssueForListing> item) {
                item.add(new NewsletterIssueLink("issueLink", item.getModelObject()));
            }
        });
    }

}
