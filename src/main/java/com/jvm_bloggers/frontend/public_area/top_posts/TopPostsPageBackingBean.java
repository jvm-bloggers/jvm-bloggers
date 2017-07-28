package com.jvm_bloggers.frontend.public_area.top_posts;

import com.jvm_bloggers.domain.query.top_posts_summary.TopPostsSummaryBasicDetails;
import com.jvm_bloggers.domain.query.top_posts_summary.TopPostsSummaryQuery;
import io.vavr.collection.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class TopPostsPageBackingBean {

    private final TopPostsSummaryQuery topPostsSummaryQuery;

    public List<TopPostsSummaryBasicDetails> getAllSummaries() {
        return topPostsSummaryQuery.loadBasicDetailsForAllSummaries();
    }

}
