package com.jvm_bloggers.frontend.admin_area;

import com.jvm_bloggers.frontend.admin_area.counters.NewPostsCounterModel;
import com.jvm_bloggers.frontend.admin_area.counters.PostsWaitingForModerationCounterModel;
import com.jvm_bloggers.frontend.admin_area.moderation.ModerationPage;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("admin")
public class AdminDashboardPage extends AbstractAdminPage {

    public AdminDashboardPage() {
        add(new Label("postsSinceLastPublication", new NewPostsCounterModel()));
        add(new Label("postsWaitingForModeration", new PostsWaitingForModerationCounterModel()));

        add(new BookmarkablePageLink<ModerationPage>(
                "moderationDetailsLink", ModerationPage.class));
    }
}
