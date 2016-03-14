package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import com.sun.syndication.feed.synd.SyndEntry;

import lombok.Data;
import lombok.NonNull;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;

@Data
public class RssEntryWithAuthor {

    @NonNull
    private final Blog blog;
    @NonNull
    private final SyndEntry rssEntry;

}
