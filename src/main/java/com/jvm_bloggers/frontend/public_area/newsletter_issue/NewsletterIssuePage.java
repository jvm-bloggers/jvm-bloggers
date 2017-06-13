package com.jvm_bloggers.frontend.public_area.newsletter_issue;

import com.jvm_bloggers.domain.query.NewsletterIssueNumber;
import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedNewsletterIssue;
import com.jvm_bloggers.frontend.public_area.AbstractFrontendPage;
import com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel.NewsletterIssuePanel;

import io.vavr.control.Option;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import static com.jvm_bloggers.domain.query.NewsletterIssueNumber.of;

@MountPath("issue")
public class NewsletterIssuePage extends AbstractFrontendPage {

    static final String ISSUE_PANEL_ID = "issuePanel";
    private static final int DEFAULT_VALUE = -1;

    @SpringBean
    private NewsletterIssuePageBackingBean backingBean;

    public NewsletterIssuePage(PageParameters parameters) {
        super(parameters);
        NewsletterIssueNumber issueNumber = of(extractIssueNumberFromParameters());
        Option<PublishedNewsletterIssue> foundIssue = backingBean.findByIssueNumber(issueNumber);

        if (foundIssue.isDefined()) {
            add(new NewsletterIssuePanel(ISSUE_PANEL_ID, foundIssue.get(),
                backingBean.findNextIssueNumber(issueNumber),
                backingBean.findPreviousIssueNumber(issueNumber)));
        } else {
            add(new Label(ISSUE_PANEL_ID, "Nie znaleziono takiego wydania"));
        }
    }

    private long extractIssueNumberFromParameters() {
        return getPageParameters().get(0).toLong(DEFAULT_VALUE);
    }

    public static PageParameters buildShowIssueParams(Long issueNumber) {
        return new PageParameters().set(0, issueNumber);
    }

    public static PageParameters buildShowIssueParams(NewsletterIssueNumber issueNumber) {
        return new PageParameters().set(0, issueNumber.asLong());
    }

    @Override
    protected String getPageTitle() {
        return "Wydanie #" + extractIssueNumberFromParameters();
    }

}
