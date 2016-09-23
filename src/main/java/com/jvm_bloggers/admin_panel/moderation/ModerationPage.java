package com.jvm_bloggers.admin_panel.moderation;

import com.jvm_bloggers.admin_panel.AbstractAdminPage;
import com.jvm_bloggers.admin_panel.panels.CustomFeedbackPanel;
import com.jvm_bloggers.admin_panel.panels.CustomPagingNavigator;
import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPost;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("admin-moderation")
@AuthorizeInstantiation(Roles.ADMIN)
public class ModerationPage extends AbstractAdminPage {

    @SpringBean
    private ModerationPageRequestHandler requestHandler;

    private final CustomFeedbackPanel feedback;

    @SpringBean
    private BlogPostItemPopulator blogPostItemPopulator;

    public ModerationPage() {
        feedback = new CustomFeedbackPanel("feedback");
        add(feedback);

        final Form<Void> moderationForm = new Form<>("moderationForm");
        moderationForm.setOutputMarkupId(true);
        add(moderationForm);
        DataView<BlogPost> dataView = new DataView<BlogPost>("pageable", requestHandler) {
            @Override
            protected void populateItem(final Item<BlogPost> item) {
                blogPostItemPopulator.populateItem(item, moderationForm, feedback);
            }
        };

        dataView.setItemsPerPage(defaultPaginationSize);
        moderationForm.add(dataView);
        moderationForm.add(new CustomPagingNavigator("navigator", dataView));
    }
}
