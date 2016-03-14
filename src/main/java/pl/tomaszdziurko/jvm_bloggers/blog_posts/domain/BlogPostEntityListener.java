package pl.tomaszdziurko.jvm_bloggers.blog_posts.domain;

import javax.persistence.PostPersist;
import javax.persistence.PostRemove;
import javax.persistence.PostUpdate;

import org.springframework.cache.CacheManager;

import pl.tomaszdziurko.jvm_bloggers.SpringApplicationContext;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.AggregatedRssFeedProducer;

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
