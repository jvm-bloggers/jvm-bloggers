package com.jvm_bloggers.core.data_fetching.blogs;

import com.jvm_bloggers.core.data_fetching.blogs.json_data.BloggerEntry;
import com.jvm_bloggers.entities.blog.Blog;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.util.Objects;

@Component
public class BloggerChangedVerifier {

    public boolean pendingChanges(Blog blog, BloggerEntry bloggerEntry) {
        return !Objects.equals(blog.getAuthor(), bloggerEntry.getName())
            || !Objects.equals(blog.getBookmarkableId(), bloggerEntry.getBookmarkableId())
            || !StringUtils.equalsIgnoreCase(blog.getRss(), bloggerEntry.getRss())
            || !Objects.equals(blog.getBlogType(), bloggerEntry.getBlogType())
            || !Objects.equals(blog.getTwitter(), bloggerEntry.getTwitter())
            || urlFromRssIsValidAndDifferentThanExistingOne(blog, bloggerEntry);
    }

    private boolean urlFromRssIsValidAndDifferentThanExistingOne(
        Blog blog,
        BloggerEntry bloggerEntry
    ) {
        return StringUtils.isNotBlank(bloggerEntry.getUrl())
            && !StringUtils.equalsIgnoreCase(blog.getUrl(), bloggerEntry.getUrl());
    }
}
