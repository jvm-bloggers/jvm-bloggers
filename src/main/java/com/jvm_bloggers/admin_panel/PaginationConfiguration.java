package com.jvm_bloggers.admin_panel;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class PaginationConfiguration {

    private final int defaultPageSize;

    @Autowired
    public PaginationConfiguration(@Value("${items.pagination.size}") int defaultPageSize) {
        this.defaultPageSize = defaultPageSize;
    }

    public int getDefaultPageSize() {
        return defaultPageSize;
    }
}
