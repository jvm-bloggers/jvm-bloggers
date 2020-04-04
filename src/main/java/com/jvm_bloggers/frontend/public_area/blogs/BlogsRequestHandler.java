package com.jvm_bloggers.frontend.public_area.blogs;

import com.jvm_bloggers.domain.query.blog_statistics_for_listing.BlogStatisticsForListing;
import com.jvm_bloggers.domain.query.blog_statistics_for_listing.BlogStatisticsForListingQuery;
import com.jvm_bloggers.entities.blog.BlogType;
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

import java.util.Iterator;

@Slf4j
public class BlogsRequestHandler implements IDataProvider<BlogStatisticsForListing> {

    @SpringBean
    private BlogStatisticsForListingQuery blogStatsForListingQuery;

    @SpringBean
    private PaginationConfiguration paginationConfiguration;

    private final BlogType blogType;

    public BlogsRequestHandler(BlogType blogType) {
        Injector.get().inject(this);
        this.blogType = blogType;
    }

    @Override
    public Iterator<? extends BlogStatisticsForListing> iterator(long first, long count) {
        log.debug("Refreshing data, first {}, count {}", first, count);
        int page = (int) (first / paginationConfiguration.getDefaultPageSize());
        long start = System.currentTimeMillis();
        Iterator<BlogStatisticsForListing> iterator = blogStatsForListingQuery
            .findBlogPostStatistics(blogType, page, paginationConfiguration.getDefaultPageSize())
            .iterator();
        long stop = System.currentTimeMillis();
        log.debug("Iterator() execution time = " + (stop - start) + " ms");
        return iterator;
    }

    @Override
    public long size() {
        long start = System.currentTimeMillis();
        long count = blogStatsForListingQuery.countByBlogType(blogType);
        long stop = System.currentTimeMillis();
        log.debug("Size() execution time = " + (stop - start) + " ms");
        return count;
    }

    @Override
    public IModel<BlogStatisticsForListing> model(BlogStatisticsForListing object) {
        return Model.of(object);
    }
}
