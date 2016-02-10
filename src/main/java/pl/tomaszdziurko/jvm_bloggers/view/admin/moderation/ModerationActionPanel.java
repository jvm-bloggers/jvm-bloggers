package pl.tomaszdziurko.jvm_bloggers.view.admin.moderation;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;

public class ModerationActionPanel extends Panel {

    @SpringBean
    private BlogPostRepository blogPostRepository;

    public ModerationActionPanel(String id, Form<Void> moderationForm, CustomFeedbackPanel feedback, IModel<? extends BlogPost> blogPostModel) {
        super(id);

        AjaxButton acceptPost = new AjaxButton("acceptPost", moderationForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                BlogPost blogPost = blogPostModel.getObject();
                blogPost.setApproved(true);
                blogPostRepository.save(blogPost);
                getSession().success("Blog post '" +  blogPost.getTitle() + "' accepted!");
                target.add(moderationForm);
                target.add(feedback);
            }

            @Override
            public boolean isVisible() {
                return !blogPostModel.getObject().isApproved();
            }
        };
        add(acceptPost);

        AjaxButton rejectPost = new AjaxButton("rejectPost", moderationForm) {
            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                BlogPost blogPost = blogPostModel.getObject();
                blogPost.setApproved(false);
                blogPostRepository.save(blogPost);
                getSession().success("Blog post '" +  blogPost.getTitle() + "' rejected!");
                target.add(moderationForm);
                target.add(feedback);
            }

            @Override
            public boolean isVisible() {
                return !blogPostModel.getObject().isRejected();
            }
        };
        add(rejectPost);
    }

}
