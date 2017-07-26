package com.jvm_bloggers.frontend.public_area.all_issues;

import com.jvm_bloggers.domain.query.newsletter_issue_for_listing.NewsletterIssueForListing;
import com.jvm_bloggers.domain.query.newsletter_issue_for_listing.NewsletterIssueForListingQuery;

import io.vavr.Tuple2;
import io.vavr.collection.Array;
import io.vavr.collection.Map;
import io.vavr.collection.Seq;
import io.vavr.collection.TreeMap;

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

    public Map<YearMonth, Seq<NewsletterIssueForListing>> getIssuesGroupedByYearMonth() {
        Seq<NewsletterIssueForListing> allIssues = query.findAllByOrderByPublishedDateDesc();

        Array<Tuple2<YearMonth, Seq<NewsletterIssueForListing>>>
            issuesGroupedByYearMonth = allIssues
            .groupBy(this::getYearMonthFrom)
            .mapValues(list ->
                list
                    .sorted(Comparator.comparing(NewsletterIssueForListing::getIssueNumber))
                    .reverse()
            )
            .map(Tuple2::new)
            .toArray();
        return TreeMap.ofEntries(
            Comparator.reverseOrder(),
            issuesGroupedByYearMonth);
    }

    private YearMonth getYearMonthFrom(NewsletterIssueForListing issue) {
        return from(issue.getPublicationDate());
    }

}
