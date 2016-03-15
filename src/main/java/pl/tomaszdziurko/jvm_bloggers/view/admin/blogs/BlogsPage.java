package pl.tomaszdziurko.jvm_bloggers.view.admin.blogs;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.spring.injection.annot.SpringBean;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.view.admin.AbstractAdminPage;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomPagingNavigator;

/**
 * @author Mateusz Urbański <matek2305@gmail.com>.
 */
@AuthorizeInstantiation(Roles.ADMIN)
public class BlogsPage extends AbstractAdminPage {

    public static final long BLOGS_PER_PAGE = 15;

    @SpringBean
    private BlogsPageRequestHandler requestHandler;

    private final CustomFeedbackPanel feedbackPanel;
    private final Form<?> form;

    public BlogsPage() {
        this.feedbackPanel = new CustomFeedbackPanel("feedback");
        this.form = new Form<>("blogDataForm");

        add(feedbackPanel);

        BlogsDataView blogsDataView = new BlogsDataView("blogsDataView", requestHandler, BLOGS_PER_PAGE);
        form.add(blogsDataView);
        form.add(new CustomPagingNavigator("navigator", blogsDataView));
        add(form);
    }

    private class BlogsDataView extends DataView<Blog> {

        private BlogsDataView(String id, IDataProvider<Blog> dataProvider, long itemsPerPage) {
            super(id, dataProvider, itemsPerPage);
        }

        @Override
        protected void populateItem(Item<Blog> item) {
            Blog blog = item.getModelObject();
            item.add(new Label("author", blog.getAuthor()));
            item.add(new ExternalLink("rss", blog.getRss(), blog.getRss()));
            item.add(new ExternalLink("twitter", blog.getTwitterUrl(), blog.getTwitter()));
            item.add(new Label("dateAdded", blog.getDateAdded().format(DATE_FORMATTER)));
            item.add(new Label("status", blog.getStatus()));
            item.add(new BlogActionPanel("actions", form, item.getModel(), feedbackPanel));
        }
    }
}
