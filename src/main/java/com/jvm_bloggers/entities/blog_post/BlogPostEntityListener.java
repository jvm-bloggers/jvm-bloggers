package com.jvm_bloggers.entities.blog_post;

import com.jvm_bloggers.SpringApplicationContext;
import com.jvm_bloggers.core.rss.AggregatedRssFeedProducer;
import org.springframework.cache.CacheManager;

import javax.persistence.PostPersist;
import javax.persistence.PostRemove;
import javax.persistence.PostUpdate;

public class BlogPostEntityListener {

    @PostPersist
    @PostUpdate
    @PostRemove
    public void invalidateRssCache(BlogPost blogPost) {
        getCacheManager().getCache(AggregatedRssFeedProducer.RSS_CACHE).clear();
    }

    private CacheManager getCacheManager() {
        return SpringApplicationContext.getBean(CacheManager.class);
    }

}
