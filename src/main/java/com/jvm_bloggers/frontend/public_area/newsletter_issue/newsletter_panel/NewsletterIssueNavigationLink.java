package com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel;

import com.jvm_bloggers.domain.query.NewsletterIssueNumber;
import com.jvm_bloggers.frontend.public_area.newsletter_issue.NewsletterIssuePage;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.model.Model;

import static com.jvm_bloggers.frontend.public_area.newsletter_issue.NewsletterIssuePage.buildShowIssueParams;
import static java.lang.String.format;

public class NewsletterIssueNavigationLink extends BookmarkablePageLink<NewsletterIssuePage> {

    enum Direction {
        NEXT("Wydanie (#%d) >>"),
        PRESIOUS("<< Wydanie (#%d)");

        private String value;
        Direction(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }

    public NewsletterIssueNavigationLink(String id, NewsletterIssueNumber issueNumber,
                                         Direction direction) {
        super(id, NewsletterIssuePage.class, buildShowIssueParams(issueNumber));
        setBody(Model.of(format(direction.getValue(), issueNumber.asLong())));
    }

}
