package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import com.google.common.base.Preconditions;
import lombok.NonNull;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

public class RssLink {

    private final Blog blog;

    public RssLink(@NonNull Blog blog) {
        Preconditions.checkArgument(isNotBlank(blog.getRss()),
                "Rss link can not be NULL nor empty.");
        this.blog = blog;
    }

    public String getUrl() {
        return blog.getRss();
    }

    public Blog getOwner() {
        return blog;
    }
}
