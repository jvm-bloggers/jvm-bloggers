package com.jvm_bloggers.entities.blog_post;

import com.jvm_bloggers.SpringApplicationContext;
import com.jvm_bloggers.core.rss.AggregatedRssFeedProducer;
import com.jvm_bloggers.domain.query.blog_statistics_for_listing.BlogStatisticsForListingQuery;

import org.springframework.cache.CacheManager;

import javax.persistence.PostPersist;
import javax.persistence.PostRemove;
import javax.persistence.PostUpdate;

public class BlogPostEntityListener {

    @PostPersist
    public void postPersistListener(BlogPost blogPost) {
        invalidateRssCache();
        invalidateBlogsStatisticsCache();
    }

    @PostUpdate
    public void postUpdateListener(BlogPost blogPost) {
        invalidateRssCache();
    }

    @PostRemove
    public void postRemoveListener(BlogPost blogPost) {
        invalidateRssCache();
        invalidateBlogsStatisticsCache();
    }

    private void invalidateRssCache() {
        getCacheManager().getCache(AggregatedRssFeedProducer.RSS_CACHE).clear();
    }

    private void invalidateBlogsStatisticsCache() {
        getCacheManager().getCache(BlogStatisticsForListingQuery.BLOG_STATISTICS_CACHE).clear();
    }

    private CacheManager getCacheManager() {
        return SpringApplicationContext.getBean(CacheManager.class);
    }

}
