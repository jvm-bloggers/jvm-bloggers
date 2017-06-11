package com.jvm_bloggers.frontend.admin_area;

import com.jvm_bloggers.frontend.admin_area.counters.NewPostsCounterModel;
import com.jvm_bloggers.frontend.admin_area.counters.PostsWaitingForModerationCounterModel;
import com.jvm_bloggers.frontend.admin_area.moderation.ModerationPage;
import com.jvm_bloggers.frontend.admin_area.panels.CustomFeedbackPanel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.StatelessForm;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("admin")
public class AdminDashboardPage extends AbstractAdminPage {

    @SpringBean
    private AdminDashboardPageBackingBean backingBean;

    public AdminDashboardPage() {
        add(new CustomFeedbackPanel("feedback"));
        add(new Label("postsSinceLastPublication", new NewPostsCounterModel()));
        add(new Label("postsWaitingForModeration", new PostsWaitingForModerationCounterModel()));

        add(new BookmarkablePageLink<ModerationPage>(
                "moderationDetailsLink", ModerationPage.class));
        add(createTopPostsForm());
    }

    private Form createTopPostsForm() {
        return new StatelessForm("generateTopPostsSummaryForm") {
            @Override
            protected void onSubmit() {
                backingBean.generateTopPostsSummary();
                getSession().info("Top posts summary generated");
                setResponsePage(AdminDashboardPage.class);
            }
        };
    }
}
