package com.jvm_bloggers.admin_panel.blogs;

import com.jvm_bloggers.admin_panel.AbstractAdminPage;
import com.jvm_bloggers.admin_panel.panels.CustomFeedbackPanel;
import com.jvm_bloggers.admin_panel.panels.CustomPagingNavigator;
import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog;
import com.jvm_bloggers.utils.DateTimeUtilities;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_TIME_FORMATTER;

@MountPath("admin-blogs")
@AuthorizeInstantiation(Roles.ADMIN)
public class BlogsPage extends AbstractAdminPage {

    @SpringBean
    private BlogsPageRequestHandler requestHandler;

    public BlogsPage() {
        CustomFeedbackPanel feedbackPanel = new CustomFeedbackPanel("feedback");
        add(feedbackPanel);
        add(createBlogDataForm(feedbackPanel));
    }

    private Form<Void> createBlogDataForm(CustomFeedbackPanel feedbackPanel) {
        Form<Void> form = new Form<>("blogDataForm");
        DataView<Blog> blogDataView = createBlogDataView(form, feedbackPanel);
        form.add(blogDataView);
        form.add(new CustomPagingNavigator("navigator", blogDataView));
        return form;
    }

    private DataView<Blog> createBlogDataView(Form<Void> form, CustomFeedbackPanel feedbackPanel) {
        return new DataView<Blog>("blogsDataView", requestHandler, defaultPaginationSize) {

            @Override
            protected void populateItem(Item<Blog> item) {
                Blog blog = item.getModelObject();
                item.add(new Label("author", blog.getAuthor()));
                item.add(createTwitterLink(blog));
                item.add(new ExternalLink("rss", blog.getRss(), blog.getRss()));
                item.add(new Label("dateAdded", blog.getDateAdded().format(DATE_TIME_FORMATTER)));
                item.add(new Label("status", blog.getStatus()));
                item.add(new BlogActionPanel("actions", form, item.getModel(), feedbackPanel));
            }
        };
    }

    private ExternalLink createTwitterLink(Blog blog) {
        ExternalLink externalLink = new ExternalLink("twitterLink",
            "https://twitter.com/" + blog.getTwitter(), blog.getTwitter());
        externalLink.setVisible(StringUtils.isNotBlank(blog.getTwitter()));
        return externalLink;
    }
}
