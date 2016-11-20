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

import java.time.format.DateTimeFormatter;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@MountPath("archivedIssues")
public class ArchivedIssuePage extends AbstractFrontendPage {

    static final String ARCHIVED_ISSUE_PANEL_ID = "archivedIssuePanel";

    static final DateTimeFormatter PUBLISHED_DATE_FORMATTER = DateTimeFormatter
        .ofPattern("dd/MM/yyyy");
    static final DateTimeFormatter ARCHIVED_ISSSUE_FORMATTER = DateTimeFormatter
        .ofPattern("MM/yyyy");

    @SpringBean
    private NewsletterIssueDtoService newsletterIssueDtoService;

    public ArchivedIssuePage() {
        Map<String, List<Link>> archiwumIssuesGroup = createArchivedMonthGroups(
            newsletterIssueDtoService);
        archiwumIssuesGroup = sortMapByKey(archiwumIssuesGroup);
        add(new ArchivedIssuePanel(ARCHIVED_ISSUE_PANEL_ID, archiwumIssuesGroup));
    }

    private Map<String, List<Link>> createArchivedMonthGroups(
        NewsletterIssueDtoService newsletterIssueDtoService) {
        return newsletterIssueDtoService
            .findAllByOrderByPublishedDateDesc().stream()
            .collect(Collectors.groupingBy(this::getArchivedIssueGroup,
                Collectors.mapping(this::getLink, Collectors.toList())));
    }

    private Map<String, List<Link>> sortMapByKey(Map<String, List<Link>> map) {
        Map<String, List<Link>> sortedMap = new LinkedHashMap<String, List<Link>>();
        map.entrySet().stream()
            .sorted(Map.Entry.<String, List<Link>>comparingByKey()
                .reversed())
            .forEachOrdered(e -> sortedMap.put(e.getKey(), e.getValue()));
        return sortedMap;
    }

    private String getArchivedIssueGroup(NewsletterIssueDto issue) {
        return ARCHIVED_ISSSUE_FORMATTER.format(issue.publishedDate);
    }

    private Link getLink(NewsletterIssueDto issue) {
        return (Link) new BookmarkablePageLink<>("issueLink", NewsletterIssuePage.class,
            NewsletterIssuePage.buildShowIssueParams(issue.number))
                .setBody(Model.of(new StringResourceModel("archived.issue.link.label")
                    .setParameters(issue.number,
                        PUBLISHED_DATE_FORMATTER.format(issue.publishedDate))));
    }
}
