package pl.tomaszdziurko.jvm_bloggers.view.admin.blogs;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.spring.injection.annot.SpringBean;

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.view.admin.AbstractAdminPage;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomPagingNavigator;

import static pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities.DATE_FORMATTER;

@AuthorizeInstantiation(Roles.ADMIN)
public class BlogsPage extends AbstractAdminPage {

    public static final long BLOGS_PER_PAGE = 15;

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
        return new DataView<Blog>("blogsDataView", requestHandler, BLOGS_PER_PAGE) {

            @Override
            protected void populateItem(Item<Blog> item) {
                Blog blog = item.getModelObject();
                item.add(new Label("author", blog.getAuthor()));
                item.add(createTwitterLink(blog));
                item.add(new ExternalLink("rss", blog.getRss(), blog.getRss()));
                item.add(new Label("dateAdded", blog.getDateAdded().format(DATE_FORMATTER)));
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
