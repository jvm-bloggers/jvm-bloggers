package com.jvm_bloggers.frontend.public_area.blogs;

import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration;
import com.jvm_bloggers.frontend.common_components.infinite_scroll.InfinitePaginationPanel;
import com.jvm_bloggers.frontend.common_components.request_handlers.BlogPostsPageRequestHandler;
import com.jvm_bloggers.frontend.public_area.AbstractFrontendPage;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import static com.jvm_bloggers.frontend.public_area.blogs.BlogPostsPage.BLOG_ID_PARAM;
import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_FORMATTER;

@MountPath("blogs/${" + BLOG_ID_PARAM + "}/posts")
public class BlogPostsPage extends AbstractFrontendPage {

    static final String AUTHOR_ID = "author";
    static final String BLOG_ID_PARAM = "blogId";
    static final String DATA_VIEW_ID = "pageable";
    static final String DATA_VIEW_WRAPPER_ID = "pageable-wrapper";
    static final String INFINITE_SCROLL_ID = "infinite-pager";
    static final String LINK_ID = "link";
    static final String PUBLISHED_DATE_ID = "date";

    @SpringBean
    PaginationConfiguration paginationConfiguration;

    @SpringBean
    BlogRepository blogRepository;

    @SpringBean
    BlogPostRepository blogPostRepository;

    @Override
    protected String getPageTitle() {
        return "Lista wpisów";
    }

    public BlogPostsPage(PageParameters parameters) {
        BlogPostsPageRequestHandler requestHandler = new BlogPostsPageRequestHandler(
            paginationConfiguration,
            blogPostRepository,
            blogRepository,
            parameters.get(BLOG_ID_PARAM).toLong(-1));

        add(new Label(AUTHOR_ID, requestHandler.getAuthor()));

        DataView dataView = createBlogPostDataView(requestHandler);
        WebMarkupContainer pageableWrapper = new WebMarkupContainer(DATA_VIEW_WRAPPER_ID);
        add(pageableWrapper);
        pageableWrapper.add(dataView);
        pageableWrapper.add(new InfinitePaginationPanel(INFINITE_SCROLL_ID,
            pageableWrapper, dataView));
    }

    private DataView<BlogPost> createBlogPostDataView(BlogPostsPageRequestHandler requestHandler) {
        final DataView<BlogPost> dataView =
            new DataView<BlogPost>(DATA_VIEW_ID, requestHandler) {
                @Override
                protected void populateItem(Item<BlogPost> item) {
                    BlogPost post = item.getModelObject();
                    item.add(new ExternalLink(LINK_ID, post.getUrl(), post.getTitle()));
                    item.add(new Label(PUBLISHED_DATE_ID,
                        post.getPublishedDate().format(DATE_FORMATTER)));
                }
            };

        dataView.setItemsPerPage(paginationConfiguration.getDefaultPageSize());
        dataView.setOutputMarkupId(true);
        return dataView;
    }
}
