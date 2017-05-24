package com.jvm_bloggers.frontend.public_area.all_issues;

import com.jvm_bloggers.domain.query.newsletter_issue_for_listing.NewsletterIssueForListing;
import com.jvm_bloggers.domain.query.newsletter_issue_for_listing.NewsletterIssueForListingQuery;
import javaslang.Tuple2;
import javaslang.collection.List;
import javaslang.collection.Map;
import javaslang.collection.TreeMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.YearMonth;
import java.util.Comparator;

import static java.time.YearMonth.from;

@Service
public class AllIssuesPageBackingBean {

    private final NewsletterIssueForListingQuery query;

    @Autowired
    public AllIssuesPageBackingBean(NewsletterIssueForListingQuery query) {
        this.query = query;
    }

    public Map<YearMonth, List<NewsletterIssueForListing>> getIssuesGroupedByYearMonth() {
        List<NewsletterIssueForListing> allIssues = query.findAllByOrderByPublishedDateDesc();

        Map<YearMonth, List<NewsletterIssueForListing>> issuesGroupedByYearMonth = allIssues
            .groupBy(this::getYearMonthFrom)
            .mapValues(list ->
                list
                    .sorted(Comparator.comparing(NewsletterIssueForListing::getIssueNumber))
                    .reverse()
            );
        return TreeMap.ofEntries(
            Comparator.reverseOrder(),
            issuesGroupedByYearMonth
                .map(Tuple2::new)
                .toArray());
    }

    private YearMonth getYearMonthFrom(NewsletterIssueForListing issue) {
        return from(issue.getPublicationDate());
    }

}
