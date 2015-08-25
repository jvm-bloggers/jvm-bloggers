package pl.tomaszdziurko.jvm_bloggers.people.json_data;

import lombok.Data;

@Data
public class BloggerEntry {
    private String name;
    private String rss;
    private String twitter;
}
