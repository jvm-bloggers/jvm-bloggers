package com.jvm_bloggers.core.data_fetching.blog_posts;

import com.jvm_bloggers.entities.blog.Blog;
import com.rometools.rome.feed.synd.SyndEntry;
import lombok.Data;
import lombok.NonNull;

@Data
public class RssEntryWithAuthor {

    @NonNull
    private final Blog blog;
    @NonNull
    private final SyndEntry rssEntry;

}
