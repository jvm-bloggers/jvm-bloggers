package com.jvm_bloggers.frontend.public_area.all_issues.all_issues_panel;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;

import java.util.List;

import static com.jvm_bloggers.utils.DateTimeUtilities.POLISH_MONTHS;

public class IssuesInMonthPanel extends Panel {

    public IssuesInMonthPanel(String id, String groupLabel, List<Link<?>> monthIssues) {
        super(id);
        add(new Label("groupLabel", convertGroupLabelName(groupLabel)));
        add(new ListView<Link<?>>("issuesList", monthIssues) {
            @Override
            protected void populateItem(ListItem<Link<?>> item) {
                item.add(item.getModelObject());
            }
        });
    }

    private String convertGroupLabelName(String groupLabel) {
        return POLISH_MONTHS[Integer.parseInt(groupLabel.substring(5)) - 1]
            + " " + groupLabel.substring(0, 4);
    }
}
