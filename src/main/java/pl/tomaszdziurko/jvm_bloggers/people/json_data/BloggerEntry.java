package pl.tomaszdziurko.jvm_bloggers.people.json_data;

import lombok.Data;

@Data
public class BloggerEntry {
    private Long jsonId;
    private String name;
    private String rss;
    private String homepage;
    private String twitter;

    public BloggerEntry() {
    }

    public BloggerEntry(Long jsonId, String name, String rss, String homepage, String twitter) {
        this.jsonId = jsonId;
        this.name = name;
        this.rss = rss;
        this.homepage = homepage;
        this.twitter = twitter;
    }

}
