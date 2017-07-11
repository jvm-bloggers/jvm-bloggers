package com.jvm_bloggers.domain.query.blog_statistics_for_listing;

import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.entities.blog.BlogType;

import javaslang.collection.List;
import lombok.AllArgsConstructor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor(onConstructor = @__(@Autowired))
public class BlogStatisticsForListingQuery {

    public static final String BLOG_STATISTICS_CACHE = "Blog posts statistics cache";

    private final BlogRepository blogRepository;

    @Cacheable(BLOG_STATISTICS_CACHE)
    public List<BlogStatisticsForListing> findBlogPostStatistics(BlogType blogType,
                                                                 Pageable pageable) {
        return blogRepository.findBlogStatistics(
            LocalDate.now().minusMonths(3).atStartOfDay(),
            LocalDate.now().minusMonths(6).atStartOfDay(),
            blogType, pageable)
            .map(BlogStatisticsForListing::fromBlogPostStatisticProjection);
    }
}
