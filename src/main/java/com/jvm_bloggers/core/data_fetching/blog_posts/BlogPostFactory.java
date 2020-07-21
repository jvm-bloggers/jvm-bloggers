package com.jvm_bloggers.core.data_fetching.blog_posts;

import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.utils.NowProvider;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Component
public class BlogPostFactory {
    private final NowProvider nowProvider;
    private final long maxNewPostAgeDays;

    @Autowired
    public BlogPostFactory(
        @Value("${max.new.post.age.days}") long maxNewPostAgeDays,
        NowProvider nowProvider) {
        this.maxNewPostAgeDays = maxNewPostAgeDays;
        this.nowProvider = nowProvider;
    }

    public BlogPost create(String title, String url, LocalDateTime publishedDate, Blog blog) {
        Boolean approved = blog.getInitialApprovedValue();
        return BlogPost.builder()
            .title(title)
            .url(url)
            .publishedDate(publishedDate)
            .approvedDate(getApprovedDate(approved, publishedDate))
            .approved(approved)
            .blog(blog)
            .build();
    }

    private LocalDateTime getApprovedDate(Boolean approved, LocalDateTime publishedDate) {
        if (!Boolean.TRUE.equals(approved)) {
            return null;
        }
        LocalDateTime now = nowProvider.now();
        return (postIsNew(publishedDate, now)) ? now : publishedDate;
    }

    private boolean postIsNew(LocalDateTime publishedDate, LocalDateTime now) {
        return publishedDate.plusDays(maxNewPostAgeDays).isAfter(now);
    }
}
