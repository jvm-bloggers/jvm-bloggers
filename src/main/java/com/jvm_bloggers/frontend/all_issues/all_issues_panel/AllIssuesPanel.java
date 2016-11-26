package com.jvm_bloggers.frontend.all_issues.all_issues_panel;

import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.RepeatingView;

import java.util.List;
import java.util.Map;

public class AllIssuesPanel extends Panel {

    public AllIssuesPanel(String id, Map<String, List<Link<?>>> allIssuesGroups) {
        super(id);
        RepeatingView issuesInMonth = new RepeatingView("issuesInMonthPanel");
        allIssuesGroups.forEach((key, value) -> issuesInMonth
            .add(new IssuesInMonthPanel(issuesInMonth.newChildId(), key, value)));
        add(issuesInMonth);
    }
}
