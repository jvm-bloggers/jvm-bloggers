package com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel;

import com.jvm_bloggers.domain.query.NewsletterIssueNumber;
import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedNewsletterIssue;

import javaslang.control.Option;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;

import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_FORMATTER;
import static org.apache.commons.lang3.StringUtils.isNotBlank;

public class NewsletterIssuePanel extends Panel {

    public NewsletterIssuePanel(String id, PublishedNewsletterIssue issue,
                                Option<NewsletterIssueNumber> nextIssueNumber,
                                Option<NewsletterIssueNumber> previousIssueNumber) {
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
        addNextLink(nextIssueNumber);
        addPreviousLink(previousIssueNumber);
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

    private void addNextLink(Option<NewsletterIssueNumber> nextIssueNumber) {
        NewsletterIssueNavigationLink next = new NewsletterIssueNavigationLink(
            "nextNewsletterIssueNumber",
            nextIssueNumber.getOrElse(NewsletterIssueNumber.of(-1L)),
            NewsletterIssueNavigationLink.Direction.NEXT);
        next.setVisible(nextIssueNumber.isDefined());
        add(next);
    }

    private void addPreviousLink(Option<NewsletterIssueNumber> previousIssueNumber) {
        NewsletterIssueNavigationLink previous = new NewsletterIssueNavigationLink(
            "previousNewsletterIssueNumber",
            previousIssueNumber.getOrElse(NewsletterIssueNumber.of(-1L)),
            NewsletterIssueNavigationLink.Direction.PRESIOUS);
        previous.setVisible(previousIssueNumber.isDefined());
        add(previous);
    }

}
