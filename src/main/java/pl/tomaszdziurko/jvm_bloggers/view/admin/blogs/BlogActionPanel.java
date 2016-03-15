package pl.tomaszdziurko.jvm_bloggers.view.admin.blogs;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;

/**
 * @author Mateusz Urbański <matek2305@gmail.com>
 */
public class BlogActionPanel extends Panel {

    @SpringBean
    private BlogRepository blogRepository;

    public BlogActionPanel(String id, Form<?> form, IModel<Blog> blogModel, CustomFeedbackPanel feedback) {
        super(id);

        AjaxButton activate = new AjaxButton("activateBlog", form) {

            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                Blog blog = blogModel.getObject();
                blog.setActive(true);
                blogRepository.save(blog);

                getSession().success("Blog activated");

                target.add(form);
                target.add(feedback);
            }
        };

        AjaxButton deactivate = new AjaxButton("deactivateBlog", form) {

            @Override
            protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
                Blog blog = blogModel.getObject();
                blog.setActive(false);
                blogRepository.save(blog);

                getSession().success("Blog deactivated");

                target.add(form);
                target.add(feedback);
            }
        };

        activate.setVisible(!blogModel.getObject().isActive());
        deactivate.setVisible(blogModel.getObject().isActive());

        add(activate);
        add(deactivate);
    }
}
