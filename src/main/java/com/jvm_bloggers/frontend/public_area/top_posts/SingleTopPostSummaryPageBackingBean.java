package com.jvm_bloggers.frontend.public_area.top_posts;

import com.jvm_bloggers.domain.query.top_posts_summary.PublishedTopPostSummary;
import com.jvm_bloggers.domain.query.top_posts_summary.TopPostsSummaryQuery;
import javaslang.control.Option;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.YearMonth;

@Service
@RequiredArgsConstructor
public class SingleTopPostSummaryPageBackingBean {

    private final TopPostsSummaryQuery topPostsSummaryQuery;

    public Option<PublishedTopPostSummary> findSummaryFor(YearMonth yearMonth) {
        return topPostsSummaryQuery.findFor(yearMonth);
    }

}
