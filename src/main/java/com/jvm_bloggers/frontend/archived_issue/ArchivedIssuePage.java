package com.jvm_bloggers.frontend.archived_issue;

import com.jvm_bloggers.frontend.AbstractFrontendPage;
import com.jvm_bloggers.frontend.archived_issue.archived_panel.ArchivedIssuePanel;
import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssueDto;
import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssueDtoService;
import com.jvm_bloggers.frontend.newsletter_issue.NewsletterIssuePage;

import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.StringResourceModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import java.util.Comparator;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.stream.Collectors;

import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_FORMATTER;
import static com.jvm_bloggers.utils.DateTimeUtilities.MONTH_FORMATTER;

@MountPath("archivedIssues")
public class ArchivedIssuePage extends AbstractFrontendPage {

    private static final String ARCHIVED_ISSUE_PANEL_ID = "archivedIssuePanel";

    @SpringBean
    private NewsletterIssueDtoService newsletterIssueDtoService;

    public ArchivedIssuePage() {
        SortedMap<String, List<Link<?>>> archivedIssuesGroup = createArchivedMonthGroups(
            newsletterIssueDtoService);
        add(new ArchivedIssuePanel(ARCHIVED_ISSUE_PANEL_ID, archivedIssuesGroup));
    }

    private SortedMap<String, List<Link<?>>> createArchivedMonthGroups(
        NewsletterIssueDtoService newsletterIssueDtoService) {
        return newsletterIssueDtoService
            .findAllByOrderByPublishedDateDesc().stream()
            .collect(Collectors.groupingBy(
                this::getArchivedIssueGroup,
                () -> new TreeMap<String, List<Link<?>>>(Comparator.reverseOrder()),
                Collectors.mapping(this::getLink, Collectors.toList())));
    }

    private String getArchivedIssueGroup(NewsletterIssueDto issue) {
        return MONTH_FORMATTER.format(issue.publishedDate);
    }

    private Link<?> getLink(NewsletterIssueDto issue) {
        return (Link<?>) new BookmarkablePageLink<>("issueLink", NewsletterIssuePage.class,
            NewsletterIssuePage.buildShowIssueParams(issue.number))
            .setBody(Model.of(new StringResourceModel("archived.issue.link.label")
                .setParameters(issue.number,
                    DATE_FORMATTER.format(issue.publishedDate))));
    }
}
