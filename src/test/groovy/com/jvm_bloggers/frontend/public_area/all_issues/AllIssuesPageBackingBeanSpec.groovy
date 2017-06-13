package com.jvm_bloggers.frontend.public_area.all_issues

import com.jvm_bloggers.domain.query.NewsletterIssueNumber
import com.jvm_bloggers.domain.query.newsletter_issue_for_listing.NewsletterIssueForListing
import com.jvm_bloggers.domain.query.newsletter_issue_for_listing.NewsletterIssueForListingQuery
import io.vavr.collection.List as VavrList
import io.vavr.collection.Seq
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDate
import java.time.Month
import java.time.YearMonth

import static java.time.Month.*

class AllIssuesPageBackingBeanSpec extends Specification {

    private NewsletterIssueForListingQuery query = Stub(NewsletterIssueForListingQuery)

    @Subject
    AllIssuesPageBackingBean backingBean = new AllIssuesPageBackingBean(query)

    def "Should group issues into YearMonth lists"() {
        given:
        query.findAllByOrderByPublishedDateDesc() >> VavrList.of(
                createIssueFor(2017, JANUARY, 20),
                createIssueFor(2017, JANUARY, 22),
                createIssueFor(2017, JANUARY, 21),
                createIssueFor(2017, JANUARY, 23),
                createIssueFor(2017, FEBRUARY, 24),
                createIssueFor(2017, FEBRUARY, 25),
                createIssueFor(2017, MARCH, 26),
                createIssueFor(2017, APRIL, 27)
        )

        when:
        def groupedIssues = backingBean.getIssuesGroupedByYearMonth()

        then:
        groupedIssues.size() == 4
        io.vavr.collection.Set<YearMonth> yearMonths = groupedIssues.keySet()
        yearMonths.eq(io.vavr.collection.LinkedHashSet.of(
                YearMonth.of(2017, APRIL),
                YearMonth.of(2017, MARCH),
                YearMonth.of(2017, FEBRUARY),
                YearMonth.of(2017, JANUARY),
        ))

        YearMonth january2017 = YearMonth.of(2017, JANUARY)
            Seq<NewsletterIssueForListing> januaryIssues = groupedIssues.get(january2017).get()
        januaryIssues.size() == 4
        januaryIssues.get(0).getIssueNumber() == NewsletterIssueNumber.of(23)
        januaryIssues.get(3).getIssueNumber() == NewsletterIssueNumber.of(20)
    }

    private NewsletterIssueForListing createIssueFor(int year, Month month, int issueNumber) {
        return new NewsletterIssueForListing(
                NewsletterIssueNumber.of(issueNumber),
                LocalDate.of(year, month.getValue(), 1)
        )
    }

}
