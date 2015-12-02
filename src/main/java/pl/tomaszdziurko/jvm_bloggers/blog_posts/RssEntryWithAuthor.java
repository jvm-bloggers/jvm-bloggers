package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import com.google.common.base.Preconditions;
import com.sun.syndication.feed.synd.SyndEntry;
import lombok.Getter;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;

@Getter
public class RssEntryWithAuthor {

    private final Blog blog;
    private final SyndEntry rssEntry;

    public RssEntryWithAuthor(Blog blog, SyndEntry rssEntry) {
        Preconditions.checkArgument(blog != null, "Blog can not be null");
        Preconditions.checkArgument(rssEntry != null, "Rss entry can not be null");
        this.blog = blog;
        this.rssEntry = rssEntry;
    }

}
