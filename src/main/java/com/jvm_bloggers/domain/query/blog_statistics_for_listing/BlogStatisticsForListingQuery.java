package com.jvm_bloggers.domain.query.blog_statistics_for_listing;

import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.entities.blog.BlogType;
import com.jvm_bloggers.utils.NowProvider;
import io.vavr.collection.List;
import lombok.AllArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class BlogStatisticsForListingQuery {

    public static final String BLOG_STATISTICS_CACHE = "Blog posts statistics cache";

    private final BlogRepository blogRepository;
    private final NowProvider nowProvider;

    @Cacheable(BLOG_STATISTICS_CACHE)
    public List<BlogStatisticsForListing> findBlogPostStatistics(BlogType blogType,
                                                                 int page, int size) {
        return blogRepository.findBlogStatistics(
            nowProvider.today().minusMonths(3).atStartOfDay(),
            nowProvider.today().minusMonths(12).atStartOfDay(),
            blogType, PageRequest.of(page, size))
            .map(BlogStatisticsForListing::fromBlogPostStatisticProjection);
    }

    public long countByBlogType(BlogType blogType) {
        return blogRepository.countByBlogType(blogType);
    }
}
