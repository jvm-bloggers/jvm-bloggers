package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import com.google.common.base.Preconditions;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;

public class RssLink {

    private final Blog blog;

    public RssLink(@NonNull Blog blog) {
        Preconditions.checkArgument(StringUtils.isNotBlank(blog.getRss()),
                "Rss link can not be null");
        this.blog = blog;
    }

    public String getUrl() {
        return blog.getRss();
    }

    public Blog getOwner() {
        return blog;
    }
}
