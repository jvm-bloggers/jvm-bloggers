package com.jvm_bloggers.frontend.newsletter_issue.newsletter_panel;

import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssueDto;
import com.jvm_bloggers.utils.DateTimeUtilities;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;


public class NewsletterIssuePanel extends Panel {

    public NewsletterIssuePanel(String id, NewsletterIssueDto issue) {
        super(id);
        add(new Label("title", "Wydanie #" + issue.number));
        add(new Label("issueDate", issue.publishedDate.format(DateTimeUtilities.DATE_FORMATTER)));
        addHeading(issue.heading);

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
                issue.getCompaniesPosts()
            )
        );
        add(new BlogPostLinksSection("linksFromVideoChannels", "Nowe nagrania", issue.getVideos()));
        add(new BlogLinksSection("newBlogs", issue.newBlogs));
        addVaria(issue.varia);
    }

    private void addVaria(String variaContent) {
        Label varia = new Label("varia", variaContent);
        varia.setEscapeModelStrings(false);
        varia.setVisible(StringUtils.isNotBlank(variaContent));
        add(varia);
    }

    private void addHeading(String headingContent) {
        Label heading = new Label("heading", headingContent);
        heading.setEscapeModelStrings(false);
        heading.setVisible(StringUtils.isNotBlank(headingContent));
        add(heading);
    }
}
