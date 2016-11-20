package com.jvm_bloggers.frontend.archiwum_issue;

import com.jvm_bloggers.frontend.AbstractFrontendPage;
import com.jvm_bloggers.frontend.archiwum_issue.archiwum_panel.ArchiwumIssuePanel;
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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@MountPath("archiwum")
public class ArchiwumIssuePage extends AbstractFrontendPage {

    static final String ARCHIWUM_ISSUE_PANEL_ID = "archiwumIssuePanel";

    static final DateTimeFormatter PUBLISHED_DATE_FORMATTER = DateTimeFormatter
        .ofPattern("dd/MM/yyyy");
    static final DateTimeFormatter ARCHIWUM_ISSSUE_FORMATTER = DateTimeFormatter
        .ofPattern("MM/yyyy");

    @SpringBean
    private NewsletterIssueDtoService newsletterIssueDtoService;

    public ArchiwumIssuePage() {
        HashMap<String, List<Link>> archiwumIssuesGroup = 
            (HashMap<String, List<Link>>) newsletterIssueDtoService
            .findAllByOrderByPublishedDateDesc().stream()
            .collect(Collectors.groupingBy(this::getArchiwumIssueGroup,
                Collectors.mapping(this::getLink, Collectors.toList())));
        Map<String, List<Link>> sortedMap = new LinkedHashMap<String, List<Link>>();
        archiwumIssuesGroup.entrySet().stream()
            .sorted(Map.Entry.<String, List<Link>>comparingByKey()
                .reversed())
            .forEachOrdered(e -> sortedMap.put(e.getKey(), e.getValue()));
        add(new ArchiwumIssuePanel(ARCHIWUM_ISSUE_PANEL_ID, sortedMap));
    }

    private String getArchiwumIssueGroup(NewsletterIssueDto issue) {
        return ARCHIWUM_ISSSUE_FORMATTER.format(issue.publishedDate);
    }

    private Link getLink(NewsletterIssueDto issue) {
        return (Link) new BookmarkablePageLink<>("issueLink", NewsletterIssuePage.class,
            NewsletterIssuePage.buildShowIssueParams(issue.number))
                .setBody(Model.of(new StringResourceModel("archiwum.issue.link.label")
                    .setParameters(issue.number,
                        PUBLISHED_DATE_FORMATTER.format(issue.publishedDate))));
    }
}
