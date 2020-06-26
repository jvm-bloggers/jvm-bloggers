package com.jvm_bloggers.frontend.public_area.blogs.single_blog;

import com.jvm_bloggers.domain.query.blog_post_for_listing.BlogPostForListing;
import com.jvm_bloggers.domain.query.blog_post_for_listing.BlogPostForListingQuery;
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.util.Iterator;

@Slf4j
public class BlogPostsPageRequestHandler implements IDataProvider<BlogPostForListing> {

    @SpringBean
    private BlogPostForListingQuery blogPostForListingQuery;

    @SpringBean
    private PaginationConfiguration paginationConfiguration;

    private final Long blogId;

    public BlogPostsPageRequestHandler(String bookmarkableId) {
        Injector.get().inject(this);
        this.blogId = blogPostForListingQuery.findBlogIdByBookmarkableId(bookmarkableId)
            .getOrElse(-1L);
    }

    @Override
    public Iterator<? extends BlogPostForListing> iterator(long first, long count) {
        log.debug("Refreshing data, first {}, count {}", first, count);
        int page = (int) (first / paginationConfiguration.getDefaultPageSize());
        long start = System.currentTimeMillis();
        Iterator<BlogPostForListing> iterator = blogPostForListingQuery
            .findBlogPosts(blogId, page, paginationConfiguration.getDefaultPageSize())
            .toJavaList()
            .iterator();
        long stop = System.currentTimeMillis();
        log.debug("Iterator() execution time = " + (stop - start) + " ms");
        return iterator;
    }

    @Override
    public long size() {
        long start = System.currentTimeMillis();
        long count = blogPostForListingQuery.countByBlogId(blogId);
        long stop = System.currentTimeMillis();
        log.debug("Size() execution time = " + (stop - start) + " ms");
        return count;
    }

    @Override
    public IModel<BlogPostForListing> model(BlogPostForListing blog) {
        return Model.of(blog);
    }

    @Override
    public void detach() {

    }

    public Long getBlogId() {
        return blogId;
    }
}
