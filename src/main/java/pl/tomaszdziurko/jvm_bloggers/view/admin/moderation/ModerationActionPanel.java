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
import pl.tomaszdziurko.jvm_bloggers.view.admin.panels.CustomFeedbackPanel;

@Slf4j
class ModerationActionPanel extends Panel {

    @SpringBean
    private BlogPostRepository blogPostRepository;

    ModerationActionPanel(String id, Form<Void> moderationForm,
                                 CustomFeedbackPanel feedback,
                                 IModel<? extends BlogPost> blogPostModel) {
        super(id);
        add(createAcceptButton(moderationForm, feedback, blogPostModel));
        add(createRejectButton(moderationForm, feedback, blogPostModel));
    }

    private AjaxButton createRejectButton(final Form<Void> moderationForm,
                                          final CustomFeedbackPanel feedback,
                                          final IModel<? extends BlogPost> blogPostModel) {
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
        return rejectPost;
    }

    private AjaxButton createAcceptButton(final Form<Void> moderationForm,
                                          final CustomFeedbackPanel feedback,
                                          final IModel<? extends BlogPost> blogPostModel) {
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
        return acceptPost;
    }

}
