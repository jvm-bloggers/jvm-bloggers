package pl.tomaszdziurko.jvm_bloggers.blogs.json_data;

import lombok.Data;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType;

@Data
public class BloggerEntry {
    private Long jsonId;
    private String name;
    private String rss;
    private String twitter;
    private BlogType blogType;

    public BloggerEntry() {
    }

    public BloggerEntry(Long jsonId, String name, String rss, String twitter) {
        this(jsonId, name, rss, twitter, null);
    }

    public BloggerEntry(Long jsonId, String name, String rss, String twitter, BlogType blogType) {
        this.jsonId = jsonId;
        this.name = name;
        this.rss = rss;
        this.twitter = twitter;
        this.blogType = blogType;
    }

}
