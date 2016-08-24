package pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import pl.tomaszdziurko.jvm_bloggers.view.front_end.AbstractFrontendPage;
import pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue.newsletter_panel.NewsletterIssuePanel;

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

}
