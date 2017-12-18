package com.jvm_bloggers.frontend.admin_area.blogs;

import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.frontend.admin_area.panels.CustomFeedbackPanel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.io.Serializable;
import java.util.function.Consumer;

import static java.lang.String.format;

public class BlogActionPanel extends Panel {

    @SpringBean
    private BlogRepository blogRepository;

    private final Form<?> form;
    private final IModel<Blog> blogModel;
    private final CustomFeedbackPanel feedbackPanel;

    public BlogActionPanel(
        String id, Form<?> form, IModel<Blog> blogModel, CustomFeedbackPanel feedbackPanel) {

        super(id);
        this.form = form;
        this.blogModel = blogModel;
        this.feedbackPanel = feedbackPanel;

        add(createActivateBlogButton());
        add(createDeactivateBlogButton());
        add(createRequireModerationBlogButton());
        add(createDoNotRequireModerationBlogButton());
    }

    private AjaxButton createActivateBlogButton() {
        AjaxButton activate = createBlogActionButton(
            "activateBlog",
            (Consumer<Blog> & Serializable) b -> b.setActive(true),
            format(
                "'%s' blog activated", blogModel.getObject().getAuthor()
            )
        );
        activate.setVisible(!blogModel.getObject().isActive());
        return activate;
    }

    private AjaxButton createDeactivateBlogButton() {
        AjaxButton deactivate = createBlogActionButton(
            "deactivateBlog",
            (Consumer<Blog> & Serializable) b -> b.setActive(false),
            format(
                "'%s' blog deactivated", blogModel.getObject().getAuthor()
            )
        );
        deactivate.setVisible(blogModel.getObject().isActive());
        return deactivate;
    }

    private AjaxButton createRequireModerationBlogButton() {
        AjaxButton requireModeration = createBlogActionButton(
            "requireModerationBlog",
            (Consumer<Blog> & Serializable) b -> b.setModerationRequired(true),
            format(
                "'%s' blog now requires moderation", blogModel.getObject().getAuthor()
            )
        );
        requireModeration.setVisible(!blogModel.getObject().isModerationRequired());
        return requireModeration;
    }

    private AjaxButton createDoNotRequireModerationBlogButton() {
        AjaxButton notRequireModeration = createBlogActionButton(
            "doNotRequireModerationBlog",
            (Consumer<Blog> & Serializable) b -> b.setModerationRequired(false),
            format(
                "'%s' blog now does not require moderation", blogModel.getObject().getAuthor()
            )
        );
        notRequireModeration.setVisible(blogModel.getObject().isModerationRequired());
        return notRequireModeration;
    }

    private AjaxButton createBlogActionButton(String id, Consumer<Blog> blogAction,
                                              String successMessage) {

        return new AjaxButton(id, form) {

            @Override
            protected void onSubmit(AjaxRequestTarget target) {
                Blog blog = blogModel.getObject();
                blogAction.accept(blog);
                blogRepository.save(blog);

                getSession().success(successMessage);

                target.add(getForm());
                target.add(feedbackPanel);
            }
        };
    }
}
