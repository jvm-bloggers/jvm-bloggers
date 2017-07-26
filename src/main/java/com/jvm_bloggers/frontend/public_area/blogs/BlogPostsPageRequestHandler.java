package com.jvm_bloggers.frontend.public_area.blogs;

import com.jvm_bloggers.domain.query.blog_post_for_listing.BlogPostForListing;
import com.jvm_bloggers.domain.query.blog_post_for_listing.BlogPostForListingQuery;
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

import java.util.Iterator;

@Slf4j
public class BlogPostsPageRequestHandler implements IDataProvider<BlogPostForListing> {

    private final BlogPostForListingQuery blogPostForListingQuery;

    private final PaginationConfiguration paginationConfiguration;

    private final Long blogId;

    public BlogPostsPageRequestHandler(
        BlogPostForListingQuery blogPostForListingQuery,
        PaginationConfiguration paginationConfiguration,
        String bookmarkableId) {
        this.blogPostForListingQuery = blogPostForListingQuery;
        this.paginationConfiguration = paginationConfiguration;
        this.blogId = blogPostForListingQuery.findBlogIdByBookmarkableId(bookmarkableId)
            .getOrElse(-1L);
    }

    @Override
    public Iterator<? extends BlogPostForListing> iterator(long first, long count) {
        log.debug("Refreshing data, first {}, count {}", first, count);
        int page = Long.valueOf(first / paginationConfiguration.getDefaultPageSize()).intValue();
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
