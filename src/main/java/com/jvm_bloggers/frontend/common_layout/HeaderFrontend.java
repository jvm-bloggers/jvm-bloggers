package com.jvm_bloggers.frontend.common_layout;

import com.googlecode.wicket.jquery.ui.markup.html.link.BookmarkablePageLink;
import com.jvm_bloggers.frontend.archived_issue.ArchivedIssuePage;

import org.apache.wicket.markup.html.panel.Panel;

public class HeaderFrontend extends Panel {

    public HeaderFrontend(String id) {
        super(id);
        add(new BookmarkablePageLink<>("archivedIssues", ArchivedIssuePage.class));
    }
}
