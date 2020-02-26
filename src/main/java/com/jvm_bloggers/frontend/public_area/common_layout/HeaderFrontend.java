package com.jvm_bloggers.frontend.public_area.common_layout;

import com.googlecode.wicket.jquery.ui.markup.html.link.BookmarkablePageLink;
import com.jvm_bloggers.frontend.public_area.AboutPage;
import com.jvm_bloggers.frontend.public_area.all_issues.AllIssuesPage;
import com.jvm_bloggers.frontend.public_area.blogs.PersonalBlogsPage;
import com.jvm_bloggers.frontend.public_area.contributors.ContributorsPage;
import com.jvm_bloggers.frontend.public_area.jvm_poland_slack.JvmPolandSlackPage;
import com.jvm_bloggers.frontend.public_area.rss.RssInformationPage;
import com.jvm_bloggers.frontend.public_area.top_posts.TopPostsPage;

import org.apache.wicket.markup.html.panel.Panel;

public class HeaderFrontend extends Panel {

    public HeaderFrontend(String id) {
        super(id);
        add(new BookmarkablePageLink<>("blogs", PersonalBlogsPage.class));
        add(new BookmarkablePageLink<>("topPosts", TopPostsPage.class));
        add(new BookmarkablePageLink<>("allIssues", AllIssuesPage.class));
        add(new BookmarkablePageLink<>("contributors", ContributorsPage.class));
        add(new BookmarkablePageLink<>("about", AboutPage.class));
        add(new BookmarkablePageLink<>("rss", RssInformationPage.class));
        add(new BookmarkablePageLink<>("jvm-poland-slack", JvmPolandSlackPage.class));
    }
}
