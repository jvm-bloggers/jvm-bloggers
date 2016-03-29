package pl.tomaszdziurko.jvm_bloggers.view.admin.moderation;

import lombok.extern.slf4j.Slf4j;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;

@Slf4j
public class ModerationActionPanel extends Panel {

    @SpringBean
    private BlogPostRepository blogPostRepository;

    public ModerationActionPanel(String id, Form<Void> moderationForm,
                                 CustomFeedbackPanel feedback,
                                 IModel<? extends BlogPost> blogPostModel,
                                 boolean panelIsDisabled) {
        super(id);

        AjaxButton acceptPost = new AjaxButton("acceptPost", moderationForm) {

            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                log.debug("Accept clicked");
                BlogPost blogPost = blogPostModel.getObject();
                blogPost.setApproved(true);
                long start = System.currentTimeMillis();
                blogPostRepository.save(blogPost);
                long stop = System.currentTimeMillis();
                log.debug("Persist approved post execution time = " + (stop - start)  + " ms");
                getSession().success("Blog post '" +  blogPost.getTitle() + "' accepted!");
                target.add(moderationForm);
                target.add(feedback);
            }
        };
        acceptPost.setVisible(!blogPostModel.getObject().isApproved());
        acceptPost.setEnabled(!panelIsDisabled);
        add(acceptPost);

        AjaxButton rejectPost = new AjaxButton("rejectPost", moderationForm) {

            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                log.debug("Reject clicked");
                BlogPost blogPost = blogPostModel.getObject();
                blogPost.setApproved(false);
                long start = System.currentTimeMillis();
                blogPostRepository.save(blogPost);
                long stop = System.currentTimeMillis();
                log.debug("Persist rejected post execution time = " + (stop - start)  + " ms");
                getSession().success("Blog post '" +  blogPost.getTitle() + "' rejected!");
                target.add(moderationForm);
                target.add(feedback);
            }
        };
        rejectPost.setVisible(!blogPostModel.getObject().isRejected());
        rejectPost.setEnabled(!panelIsDisabled);
        add(rejectPost);
    }

}
