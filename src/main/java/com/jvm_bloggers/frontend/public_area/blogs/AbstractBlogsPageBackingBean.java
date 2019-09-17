package com.jvm_bloggers.frontend.public_area.blogs;

import com.jvm_bloggers.domain.query.blog_statistics_for_listing.BlogStatisticsForListing;
import com.jvm_bloggers.entities.blog.BlogType;
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration;
import lombok.RequiredArgsConstructor;
import org.apache.wicket.markup.repeater.Item;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class AbstractBlogsPageBackingBean {

    private final BlogWithStatisticsItemPopulator blogWithStatisticsItemPopulator;

    private final PaginationConfiguration paginationConfiguration;

    public int defaultPageSize() {
        return paginationConfiguration.getDefaultPageSize();
    }

    public void itemPopulator(Item<BlogStatisticsForListing> item) {
        blogWithStatisticsItemPopulator.populateItem(item);
    }

    public BlogsRequestHandler requestHandler(BlogType blogType) {
        return new BlogsRequestHandler(blogType);
    }
}
