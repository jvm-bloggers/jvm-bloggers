package com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel;

import com.googlecode.wicket.jquery.ui.markup.html.link.BookmarkablePageLink;
import com.jvm_bloggers.domain.query.NewsletterIssueNumber;
import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedNewsletterIssue;
import com.jvm_bloggers.frontend.public_area.newsletter_issue.NewsletterIssuePage;
import io.vavr.control.Option;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;

import static com.jvm_bloggers.domain.query.NewsletterIssueNumber.of;
import static com.jvm_bloggers.frontend.public_area.newsletter_issue.NewsletterIssuePage.buildShowIssueParams;
import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_FORMATTER;
import static com.jvm_bloggers.utils.HtmlEmptinessChecker.isNotEmpty;

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
        add(new BlogPostLinksSection("linksFromPodcastChannels", "Nowe podcasty", issue.getPodcasts()));
        add(new BlogPostLinksSection("linksFromVideoChannels", "Nowe prezentacje", issue.getPresentations()));
        add(new BlogLinksSection("newBlogs", issue.getNewBlogs()));
        addVaria(issue.getVariaSection());
        addNextLink(nextIssueNumber);
        addPreviousLink(previousIssueNumber);
    }

    private void addVaria(String variaContent) {
        Label varia = new Label("varia", variaContent);
        varia.setEscapeModelStrings(false);
        varia.setVisible(isNotEmpty(variaContent));
        add(varia);
    }

    private void addHeading(String headingContent) {
        Label heading = new Label("heading", headingContent);
        heading.setEscapeModelStrings(false);
        heading.setVisible(isNotEmpty(headingContent));
        add(heading);
    }

    private void addNextLink(Option<NewsletterIssueNumber> nextIssueNumber) {
        BookmarkablePageLink<NewsletterIssuePage> next = new BookmarkablePageLink<>(
            "nextNewsletterIssueNumber",
            NewsletterIssuePage.class,
            buildShowIssueParams(nextIssueNumber.getOrElse(of(-1L)))
        );
        next.setVisible(nextIssueNumber.isDefined());
        add(next);
    }

    private void addPreviousLink(Option<NewsletterIssueNumber> previousIssueNumber) {
        BookmarkablePageLink<NewsletterIssuePage> previous = new BookmarkablePageLink<>(
            "previousNewsletterIssueNumber",
            NewsletterIssuePage.class,
            buildShowIssueParams(previousIssueNumber.getOrElse(of(-1L)))
        );
        previous.setVisible(previousIssueNumber.isDefined());
        add(previous);
    }

}
