package com.jvm_bloggers.frontend.public_area.all_issues;

import com.jvm_bloggers.domain.query.newsletter_issue_for_listing.NewsletterIssueForListing;
import com.jvm_bloggers.frontend.common_components.NewsletterIssueLink;

import io.vavr.collection.Seq;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;

import java.time.YearMonth;
import java.util.Locale;

import static java.time.format.TextStyle.FULL_STANDALONE;

class IssuesInMonthPanel extends Panel {

    private static final Locale POLISH_LOCALE = new Locale("PL");

    IssuesInMonthPanel(String id, YearMonth yearMonth,
                       Seq<NewsletterIssueForListing> monthIssues) {
        super(id);
        add(new Label("groupLabel", stringRepresentationOf(yearMonth)));
        add(new ListView<NewsletterIssueForListing>("issuesList", monthIssues.toJavaList()) {
            @Override
            protected void populateItem(ListItem<NewsletterIssueForListing> item) {
                item.add(new NewsletterIssueLink("issueLink", item.getModelObject()));
            }
        });
    }

    private String stringRepresentationOf(YearMonth yearMonth) {
        String monthName = yearMonth.getMonth().getDisplayName(FULL_STANDALONE, POLISH_LOCALE);
        return String.format("%s %d", monthName, yearMonth.getYear());
    }

}
