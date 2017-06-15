package com.jvm_bloggers.frontend.admin_area.moderation;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.frontend.admin_area.panels.CustomFeedbackPanel;
import com.jvm_bloggers.utils.NowProvider;
import lombok.extern.slf4j.Slf4j;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
class ModerationActionPanel extends Panel {

    @SpringBean
    private BlogPostRepository blogPostRepository;

    @SpringBean
    private NowProvider nowProvider;

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
            protected void onSubmit(AjaxRequestTarget target) {
                log.debug("Reject clicked");
                BlogPost blogPost = blogPostModel.getObject();
                blogPost.reject();
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
            protected void onSubmit(AjaxRequestTarget target) {
                log.debug("Accept clicked");
                BlogPost blogPost = blogPostModel.getObject();
                blogPost.approve(nowProvider.now());
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
