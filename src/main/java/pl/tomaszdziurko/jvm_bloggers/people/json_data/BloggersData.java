package pl.tomaszdziurko.jvm_bloggers.people.json_data;

import lombok.Data;

import java.util.List;

@Data
public class BloggersData {
    private List<BloggerEntry> bloggers;

    public BloggersData(List<BloggerEntry> bloggers) {
        this.bloggers = bloggers;
    }

    public BloggersData() {
    }
}