package com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel;

import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedNewsletterIssue;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;

import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_FORMATTER;
import static org.apache.commons.lang3.StringUtils.isNotBlank;

public class NewsletterIssuePanel extends Panel {

    public NewsletterIssuePanel(String id, PublishedNewsletterIssue issue) {
        super(id);
        add(new Label("title", "Wydanie #" + issue.getNumber().asLong()));
        add(new Label("issueDate", issue.getPublishedDate().format(DATE_FORMATTER)));
        addHeading(issue.getHeadingSection());
        add(
            new BlogPostLinksSection(
                "linksFromPersonalBlogs",
                "Wpisy programistów",
                issue.getPersonalPosts()
            )
        );
        add(
            new BlogPostLinksSection(
                "linksFromCompanyBlogs",
                "Wpisy z blogów firmowych",
                issue.getCompanyPosts()
            )
        );
        add(new BlogPostLinksSection("linksFromVideoChannels", "Nowe nagrania", issue.getVideos()));
        add(new BlogLinksSection("newBlogs", issue.getNewBlogs()));
        addVaria(issue.getVariaSection());
    }

    private void addVaria(String variaContent) {
        Label varia = new Label("varia", variaContent);
        varia.setEscapeModelStrings(false);
        varia.setVisible(isNotBlank(variaContent));
        add(varia);
    }

    private void addHeading(String headingContent) {
        Label heading = new Label("heading", headingContent);
        heading.setEscapeModelStrings(false);
        heading.setVisible(isNotBlank(headingContent));
        add(heading);
    }

}
