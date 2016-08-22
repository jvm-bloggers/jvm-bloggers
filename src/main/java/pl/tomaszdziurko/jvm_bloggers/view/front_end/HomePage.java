package pl.tomaszdziurko.jvm_bloggers.view.front_end;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.spring.injection.annot.SpringBean;

import pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue.NewsletterIssueDto;
import pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue.NewsletterIssueDtoService;
import pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue.newsletter_panel.NewsletterIssuePanel;

import java.util.Optional;

public class HomePage extends AbstractFrontendPage {

    @SpringBean
    private NewsletterIssueDtoService newsletterIssueDtoService;

    public HomePage() {

        Optional<NewsletterIssueDto> latestIssue = newsletterIssueDtoService.getLatestIssue();
        if (latestIssue.isPresent()) {
            add(new NewsletterIssuePanel("latestIssuePanel", latestIssue.get()));
        } else {
            add(new Label("latestIssuePanel", "Nie znaleziono takiego wydania"));
        }
    }

}
