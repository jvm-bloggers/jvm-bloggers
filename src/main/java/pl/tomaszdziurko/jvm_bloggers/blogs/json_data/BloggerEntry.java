package pl.tomaszdziurko.jvm_bloggers.blogs.json_data;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class BloggerEntry {

    private Long jsonId;
    private String name;
    private String rss;
    private String url;
    private String twitter;
    private BlogType blogType;

    public BloggerEntry(Long jsonId, String name, String rss, String twitter, BlogType blogType) {
        this(jsonId, name, rss, null, twitter, blogType);
    }
}
