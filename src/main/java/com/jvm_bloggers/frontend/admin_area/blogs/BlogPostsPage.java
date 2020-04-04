package com.jvm_bloggers.frontend.admin_area.blogs;

import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.frontend.admin_area.AbstractAdminPage;
import com.jvm_bloggers.frontend.admin_area.panels.CustomPagingNavigator;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import static com.jvm_bloggers.frontend.admin_area.blogs.BlogPostsPage.BLOG_ID_PARAM;
import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_TIME_FORMATTER;
import static org.apache.commons.lang3.StringUtils.abbreviate;

@MountPath("admin-blogs/${" + BLOG_ID_PARAM + "}/posts")
public class BlogPostsPage extends AbstractAdminPage {
    static final String BLOG_ID_PARAM = "blogId";
    static final String HEADER_ID = "header";

    @SpringBean
    private BlogPostRepository blogPostRepository;

    @SpringBean
    private BlogRepository blogRepository;

    private final BlogPostsPageRequestHandler requestHandler;

    public BlogPostsPage(PageParameters parameters) {
        requestHandler = new BlogPostsPageRequestHandler(
            paginationConfiguration,
            blogPostRepository,
            blogRepository,
            parameters.get(BLOG_ID_PARAM).toLong(-1));

        DataView<BlogPost> blogPotsDataView = createBlogPostDataView();
        add(new Label(HEADER_ID, requestHandler.getPageHeader()));
        add(blogPotsDataView);
        add(new CustomPagingNavigator("navigator", blogPotsDataView));
    }

    private DataView<BlogPost> createBlogPostDataView() {
        return new DataView<>(
            "blogPostsDataView", requestHandler, defaultPaginationSize) {

            @Override
            protected void populateItem(Item<BlogPost> item) {
                BlogPost post = item.getModelObject();
                item.add(new Label("title", post.getTitle()));
                item.add(new Label("author", post.getBlog().getAuthor()));
                item.add(new ExternalLink("link", post.getUrl(), abbreviate(post.getUrl(), 90)));
                item.add(new Label("date", post.getPublishedDate().format(DATE_TIME_FORMATTER)));
                item.add(new Label("approved", post.getApprovalState()));
            }
        };
    }
}
