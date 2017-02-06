package com.jvm_bloggers.core.data_fetching.blogs.json_data;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class BloggersData {
    private List<BloggerEntry> bloggers;

    public BloggersData(List<BloggerEntry> bloggers) {
        this.bloggers = bloggers;
    }

    public BloggersData() {
        this(new ArrayList<>());
    }
}
