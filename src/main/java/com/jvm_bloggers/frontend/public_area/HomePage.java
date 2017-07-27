package com.jvm_bloggers.frontend.public_area;

import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedNewsletterIssue;
import com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel.NewsletterIssuePanel;

import io.vavr.control.Option;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class HomePage extends AbstractFrontendPage {

    static final String LATEST_ISSUE_PANEL_ID = "latestIssuePanel";

    @SpringBean
    private HomePageBackingBean backingBean;

    public HomePage() {
        Option<PublishedNewsletterIssue> latestIssue = backingBean.getLatestIssue();
        if (latestIssue.isDefined()) {
            add(new NewsletterIssuePanel(LATEST_ISSUE_PANEL_ID, latestIssue.get(),
                backingBean.findNextIssueNumber(latestIssue.get().getNumber()),
                backingBean.findPreviousIssueNumber(latestIssue.get().getNumber())));
        } else {
            add(new Label(LATEST_ISSUE_PANEL_ID, "Nie istnieje żadne wydanie newslettera."));
        }
    }

    @Override
    protected String getPageTitle() {
        return "Najnowsze wydanie newslettera";
    }

}
