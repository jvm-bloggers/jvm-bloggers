package pl.tomaszdziurko.jvm_bloggers.blogs.json_data;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class BloggerEntry {

    private Long jsonId;
    private String name;
    private String rss;
    private String url;
    private String twitter;
    private BlogType blogType;

}
