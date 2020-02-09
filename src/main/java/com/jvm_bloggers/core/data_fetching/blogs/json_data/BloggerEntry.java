package com.jvm_bloggers.core.data_fetching.blogs.json_data;

import com.jvm_bloggers.entities.blog.BlogType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class BloggerEntry {

    private String bookmarkableId;
    private String name;
    private String rss;
    private String url;
    private String twitter;
    private BlogType blogType;

    public BloggerEntry(
        String bookmarkableId,
        String name,
        String rss,
        String twitter,
        BlogType blogType
    ) {
        this(bookmarkableId, name, rss, null, twitter, blogType);
    }

    public boolean hasRss() {
        return !rss.isEmpty();
    }
}
