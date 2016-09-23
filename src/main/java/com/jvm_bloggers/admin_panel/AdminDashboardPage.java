package com.jvm_bloggers.admin_panel;

import com.jvm_bloggers.admin_panel.counters.NewPostsCounterModel;
import com.jvm_bloggers.admin_panel.counters.PostsWaitingForModerationCounterModel;
import com.jvm_bloggers.admin_panel.moderation.ModerationPage;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("admin")
@AuthorizeInstantiation(Roles.ADMIN)
public class AdminDashboardPage extends AbstractAdminPage {

    public AdminDashboardPage() {
        add(new Label("postsSinceLastPublication", new NewPostsCounterModel()));
        add(new Label("postsWaitingForModeration", new PostsWaitingForModerationCounterModel()));

        add(new BookmarkablePageLink<ModerationPage>(
                "moderationDetailsLink", ModerationPage.class));
    }
}
