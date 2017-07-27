package com.jvm_bloggers.domain.query.top_posts_summary;

import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedPost;
import io.vavr.collection.List;
import lombok.Value;

import java.time.YearMonth;

@Value
public class PublishedTopPostSummary {

    private final YearMonth yearMonth;
    private final List<PublishedPost> topPersonalPosts;
    private final List<PublishedPost> topCompanyPosts;

}
