package com.jvm_bloggers.frontend.common_components;

import com.jvm_bloggers.domain.query.NewsletterIssueNumber;
import com.jvm_bloggers.domain.query.newsletter_issue_for_listing.NewsletterIssueForListing;
import com.jvm_bloggers.frontend.public_area.newsletter_issue.NewsletterIssuePage;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.model.Model;

import java.time.LocalDate;

import static com.jvm_bloggers.frontend.public_area.newsletter_issue.NewsletterIssuePage.buildShowIssueParams;
import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_FORMATTER;
import static java.lang.String.format;

public class NewsletterIssueLink extends BookmarkablePageLink<NewsletterIssuePage> {

    private static final String LABEL = "Wydanie #%d - %s";

    public NewsletterIssueLink(
        String id,
        NewsletterIssueNumber issueNumber,
        LocalDate publicationDate) {
        super(id, NewsletterIssuePage.class, buildShowIssueParams(issueNumber));
        setBody(Model.of(format(
            LABEL,
            issueNumber.asLong(),
            DATE_FORMATTER.format(publicationDate)))
        );
    }

    public NewsletterIssueLink(String id, NewsletterIssueForListing issue) {
        this(id, issue.getIssueNumber(), issue.getPublicationDate());
    }

}
