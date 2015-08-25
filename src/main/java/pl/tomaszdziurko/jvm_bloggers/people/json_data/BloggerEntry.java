package pl.tomaszdziurko.jvm_bloggers.people.json_data;

import lombok.Data;

@Data
public class BloggerEntry {
    private String name;
    private String rss;
    private String twitter;

    public BloggerEntry() {
    }

    public BloggerEntry(String name, String rss, String twitter) {
        this.name = name;
        this.rss = rss;
        this.twitter = twitter;
    }
}
