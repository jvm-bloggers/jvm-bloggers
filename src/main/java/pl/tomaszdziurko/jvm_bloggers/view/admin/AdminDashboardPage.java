package pl.tomaszdziurko.jvm_bloggers.view.admin;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.wicketstuff.annotation.mount.MountPath;

import pl.tomaszdziurko.jvm_bloggers.view.admin.counters.NewPostsCounterModel;
import pl.tomaszdziurko.jvm_bloggers.view.admin.counters.PostsWaitingForModerationCounterModel;
import pl.tomaszdziurko.jvm_bloggers.view.admin.moderation.ModerationPage;

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
