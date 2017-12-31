package com.jvm_bloggers.frontend.admin_area.blogs;

import com.google.common.collect.Lists;
import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.frontend.admin_area.AbstractAdminPage;
import com.jvm_bloggers.frontend.admin_area.panels.CustomFeedbackPanel;
import com.jvm_bloggers.frontend.admin_area.panels.CustomPagingNavigator;
import com.jvm_bloggers.frontend.admin_area.panels.tables.BootstrapTable;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.DataTable;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.danekja.java.util.function.serializable.SerializableBiFunction;
import org.wicketstuff.annotation.mount.MountPath;

import java.util.List;

import static com.jvm_bloggers.frontend.admin_area.blogs.BlogPostsPage.BLOG_ID_PARAM;
import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_TIME_FORMATTER;

@MountPath("admin-blogs")
public class BlogsPage extends AbstractAdminPage {

    protected static final String BLOG_DATA_FORM_ID = "blogDataForm";
    protected static final String BLOGS_DATA_TABLE_ID = "blogsDataTable";
    private static final String POSTS_LINK_FRAGMENT = "postsLinkFragment";
    protected static final String POSTS_LINK = "postsLink";
    private static final String NEW_WINDOW_LINK_FRAGMENT = "newWindowLinkFragment";
    private static final String NEW_WINDOW_LINK = "newWindowLink";

    public BlogsPage() {
        CustomFeedbackPanel feedbackPanel = new CustomFeedbackPanel("feedback");
        add(feedbackPanel);
        add(createBlogDataForm(feedbackPanel));
    }

    private Form<Void> createBlogDataForm(CustomFeedbackPanel feedbackPanel) {
        Form<Void> form = new Form<>(BLOG_DATA_FORM_ID);

        DataTable<Blog, String> blogDataTable = createBlogDataTable(form, feedbackPanel);
        form.add(blogDataTable);

        form.add(new CustomPagingNavigator("navigator", blogDataTable));
        return form;
    }

    private DataTable<Blog, String> createBlogDataTable(Form<Void> form,
                                                        CustomFeedbackPanel feedbackPanel) {
        final List<IColumn<Blog, String>> columns = prepareColumns(form, feedbackPanel);
        return new BootstrapTable<>(BLOGS_DATA_TABLE_ID, columns, new BlogsPageRequestHandler(),
            defaultPaginationSize);
    }

    private List<IColumn<Blog, String>> prepareColumns(Form<Void> form,
                                                       CustomFeedbackPanel feedbackPanel) {
        return Lists.newArrayList(
            new BlogsPageColumn("Author", "author",
                (componentId, model) -> new Label(componentId, model.map(Blog::getAuthor))),
            new BlogsPageColumn("RSS", "rss",
                (componentId, model) -> new RssLinkFragment(componentId,
                    model.getObject().getRss())),
            new BlogsPageColumn("Twitter", "twitter",
                (componentId, model) -> new TwitterLinkFragment(componentId,
                    model.getObject().getTwitter())),
            new BlogsPageColumn("Date added", "dateAdded",
                (componentId, model) -> new Label(componentId,
                    model.getObject().getDateAdded().format(DATE_TIME_FORMATTER)), true),
            new BlogsPageColumn("Status", "active",
                (componentId, model) -> new Label(componentId, model.getObject().getStatus()),
                true),
            new BlogsPageColumn("Actions", null,
                (componentId, model) -> new BlogActionPanel(componentId, form, model,
                    feedbackPanel), true),
            new BlogsPageColumn("Posts", null,
                (componentId, model) -> new PostsLinkFragment(componentId,
                    model.getObject().getId()), true));
    }

    private static class BlogsPageColumn extends AbstractColumn<Blog, String> {

        private final SerializableBiFunction<String, IModel<Blog>, Component> generator;
        private final boolean centered;

        private BlogsPageColumn(String columnName, String sortProperty,
                                SerializableBiFunction<String, IModel<Blog>, Component> generator,
                                boolean centered) {
            super(new Model<>(columnName), sortProperty);
            this.generator = generator;
            this.centered = centered;
        }

        private BlogsPageColumn(String columnName, String sortProperty,
                                SerializableBiFunction<String, IModel<Blog>, Component> generator) {
            this(columnName, sortProperty, generator, false);
        }

        @Override
        public void populateItem(Item<ICellPopulator<Blog>> cellItem, String componentId,
                                 IModel<Blog> rowModel) {
            cellItem.add(generator.apply(componentId, rowModel));
        }

        @Override
        public String getCssClass() {
            if (centered) {
                return "text-center";
            } else {
                return null;
            }
        }
    }

    private class TwitterLinkFragment extends Fragment {

        private static final String TWITTER_BASE_URL = "https://twitter.com/";

        private TwitterLinkFragment(String id, String userName) {
            super(id, NEW_WINDOW_LINK_FRAGMENT, BlogsPage.this);
            add(new ExternalLink(NEW_WINDOW_LINK, TWITTER_BASE_URL + userName, userName));
            setVisible(StringUtils.isNotBlank(userName));
        }
    }

    private class RssLinkFragment extends Fragment {
        private RssLinkFragment(String id, String rss) {
            super(id, NEW_WINDOW_LINK_FRAGMENT, BlogsPage.this);
            add(new ExternalLink(NEW_WINDOW_LINK, rss, rss));
        }
    }

    private class PostsLinkFragment extends Fragment {
        private PostsLinkFragment(String id, Long blogId) {
            super(id, POSTS_LINK_FRAGMENT, BlogsPage.this);
            BookmarkablePageLink<Object> link = new BookmarkablePageLink<>(
                POSTS_LINK, BlogPostsPage.class,
                new PageParameters().add(BLOG_ID_PARAM, blogId));
            add(link);
        }
    }
}
