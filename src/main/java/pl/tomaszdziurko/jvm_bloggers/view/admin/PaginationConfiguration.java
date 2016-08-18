package pl.tomaszdziurko.jvm_bloggers.view.admin;

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
