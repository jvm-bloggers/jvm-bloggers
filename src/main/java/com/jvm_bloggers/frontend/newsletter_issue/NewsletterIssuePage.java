package com.jvm_bloggers.frontend.newsletter_issue;

import com.jvm_bloggers.frontend.AbstractFrontendPage;
import com.jvm_bloggers.frontend.newsletter_issue.newsletter_panel.NewsletterIssuePanel;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import java.util.Optional;

@MountPath("issue")
public class NewsletterIssuePage extends AbstractFrontendPage {

    static final String ISSUE_PANEL_ID = "issuePanel";

    @SpringBean
    private NewsletterIssueDtoService newsletterIssueDtoService;

    public NewsletterIssuePage(PageParameters parameters) {
        super(parameters);
        long issueNumber = parameters.get(0).toLong(-1);
        Optional<NewsletterIssueDto>
            foundIssue =
            newsletterIssueDtoService.findByIssueNumber(issueNumber);

        if (foundIssue.isPresent()) {
            add(new NewsletterIssuePanel(ISSUE_PANEL_ID, foundIssue.get()));
        } else {
            add(new Label(ISSUE_PANEL_ID, "Nie znaleziono takiego wydania"));
        }
    }

    public static PageParameters buildShowIssueParams(Long issueNumber) {
        return new PageParameters().set(0, issueNumber);
    }
}
