package com.jvm_bloggers.domain.query.blog_statistics_for_listing;

import com.jvm_bloggers.entities.blog.projections.BlogStatisticsProjection;

import io.vavr.control.Option;
import lombok.Value;

import java.io.Serializable;

@Value
public class BlogStatisticsForListing implements Serializable {

    private String bookmarkableId;
    private String url;
    private String author;
    private Option<String> twitter;
    private Integer countFirstRange;
    private Integer countSecondRange;

    public static BlogStatisticsForListing fromBlogPostStatisticProjection(
        BlogStatisticsProjection projection) {
        return new BlogStatisticsForListing(
            projection.getBookmarkableId(),
            projection.getUrl(),
            projection.getAuthor(),
            Option.of(projection.getTwitter()),
            projection.getFirstCount(),
            projection.getSecondCount()
        );
    }
}
