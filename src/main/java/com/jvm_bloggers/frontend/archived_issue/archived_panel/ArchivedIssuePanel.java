package com.jvm_bloggers.frontend.archived_issue.archived_panel;

import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.RepeatingView;

import java.util.List;
import java.util.Map;

public class ArchivedIssuePanel extends Panel {

    public ArchivedIssuePanel(String id, Map<String, List<Link>> archivedIssueGroups) {
        super(id);

        RepeatingView issuesInMonth = new RepeatingView("issuesInMonthPanel");
        archivedIssueGroups.forEach((key, value) -> issuesInMonth
            .add(new IssuesInMonthPanel(issuesInMonth.newChildId(), key, value)));
        add(issuesInMonth);

    }

}
