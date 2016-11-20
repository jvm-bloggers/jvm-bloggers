package com.jvm_bloggers.frontend.archiwum_issue.archiwum_panel;

import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.RepeatingView;

import java.util.List;
import java.util.Map;

public class ArchiwumIssuePanel extends Panel {

    public ArchiwumIssuePanel(String id, Map<String, List<Link>> archiwumIssueGroups) {
        super(id);

        RepeatingView issuesInMonth = new RepeatingView("issuesInMonthPanel");
        archiwumIssueGroups.forEach((key, value) -> issuesInMonth
            .add(new IssuesInMonthPanel(issuesInMonth.newChildId(), key, value)));
        add(issuesInMonth);

    }

}
