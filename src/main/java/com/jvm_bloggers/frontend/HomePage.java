package com.jvm_bloggers.frontend;

import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssueDto;
import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssueDtoService;
import com.jvm_bloggers.frontend.newsletter_issue.newsletter_panel.NewsletterIssuePanel;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.util.Optional;

public class HomePage extends AbstractFrontendPage {

    static final String LATEST_ISSUE_PANEL_ID = "latestIssuePanel";

    @SpringBean
    private NewsletterIssueDtoService newsletterIssueDtoService;

    public HomePage() {

        Optional<NewsletterIssueDto> latestIssue = newsletterIssueDtoService.getLatestIssue();
        if (latestIssue.isPresent()) {
            add(new NewsletterIssuePanel(LATEST_ISSUE_PANEL_ID, latestIssue.get()));
        } else {
            add(new Label(LATEST_ISSUE_PANEL_ID, "Nie znaleziono takiego wydania"));
        }
    }

}
