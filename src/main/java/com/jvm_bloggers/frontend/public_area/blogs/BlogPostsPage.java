package com.jvm_bloggers.frontend.public_area.blogs;

import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration;
import com.jvm_bloggers.frontend.admin_area.blogs.BlogPostsPageRequestHandler;
import com.jvm_bloggers.frontend.public_area.AbstractFrontendPage;
import com.jvm_bloggers.frontend.public_area.blogs.timeline_panel.BlogPostsTimelinePanel;

import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import static com.jvm_bloggers.frontend.public_area.blogs.BlogPostsPage.BLOG_ID_PARAM;

@MountPath("blogs/${" + BLOG_ID_PARAM + "}/posts")
public class BlogPostsPage extends AbstractFrontendPage {

    static final String BLOG_ID_PARAM = "blogId";
    static final String TIMELINE_ID = "timeline";

    @SpringBean
    PaginationConfiguration paginationConfiguration;

    @SpringBean
    BlogRepository blogRepository;

    @SpringBean
    BlogPostRepository blogPostRepository;

    @Override
    protected String getPageTitle() {
        return "Wpisy";
    }

    public BlogPostsPage(PageParameters parameters) {
        BlogPostsPageRequestHandler requestHandler = new BlogPostsPageRequestHandler(
            paginationConfiguration,
            blogPostRepository,
            blogRepository,
            parameters.get(BLOG_ID_PARAM).toLong(-1));
        add(new BlogPostsTimelinePanel(TIMELINE_ID, requestHandler));
    }
}
