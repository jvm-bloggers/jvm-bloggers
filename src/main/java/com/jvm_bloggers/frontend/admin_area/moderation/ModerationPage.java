package com.jvm_bloggers.frontend.admin_area.moderation;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.frontend.admin_area.AbstractAdminPage;
import com.jvm_bloggers.frontend.admin_area.panels.CustomFeedbackPanel;
import com.jvm_bloggers.frontend.admin_area.panels.CustomPagingNavigator;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("admin-moderation")
public class ModerationPage extends AbstractAdminPage {

    @SpringBean
    private ModerationPageRequestHandler requestHandler;

    private final CustomFeedbackPanel feedback;

    @SpringBean
    private BlogPostItemPopulator blogPostItemPopulator;

    public ModerationPage() {
        feedback = new CustomFeedbackPanel("feedback");
        add(feedback);

        add(new FetchingDataPanel(feedback));

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
