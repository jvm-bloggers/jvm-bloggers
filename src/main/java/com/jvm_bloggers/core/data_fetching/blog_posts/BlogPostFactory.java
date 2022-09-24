package com.jvm_bloggers.core.data_fetching.blog_posts;

import com.jvm_bloggers.core.utils.EmojiRemover;
import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog_post.BlogPost;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Component
class BlogPostFactory {

    private final EmojiRemover remover;

    @Autowired
    BlogPostFactory() {
        this(new EmojiRemover());
    }

    BlogPostFactory(final EmojiRemover remover) {
        this.remover = remover;
    }

    BlogPost create(String title, String url, LocalDateTime publishedDate, Blog blog) {
        final var approved = blog.getInitialApprovedValue();

        return BlogPost.builder()
            .title(remover.remove(title))
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
        return publishedDate;
    }
}
