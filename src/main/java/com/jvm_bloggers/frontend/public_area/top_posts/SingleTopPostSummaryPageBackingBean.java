package com.jvm_bloggers.frontend.public_area.top_posts;

import com.jvm_bloggers.domain.query.top_posts_summary.PublishedTopPostSummary;
import com.jvm_bloggers.domain.query.top_posts_summary.TopPostsSummaryQuery;
import com.jvm_bloggers.frontend.public_area.social_meta_data.SocialMetaData;
import io.vavr.control.Option;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.YearMonth;

@Service
@NoArgsConstructor
public class SingleTopPostSummaryPageBackingBean {

    private TopPostsSummaryQuery topPostsSummaryQuery;
    private String applicationBaseUrl;

    @Autowired
    public SingleTopPostSummaryPageBackingBean(
        TopPostsSummaryQuery topPostsSummaryQuery,
        @Value("${application.baseUrl}") String applicationBaseUrl
    ) {
        this.topPostsSummaryQuery = topPostsSummaryQuery;
        this.applicationBaseUrl = applicationBaseUrl;
    }

    public Option<PublishedTopPostSummary> findSummaryFor(YearMonth yearMonth) {
        return topPostsSummaryQuery.findFor(yearMonth);
    }

    public SocialMetaData createSocialMetaTags(YearMonth yearMonth) {
        return new TopPostSummarySocialMetaData(yearMonth, applicationBaseUrl);
    }

}
