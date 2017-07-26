package com.jvm_bloggers.domain.query;

import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

import java.io.Serializable;

@RequiredArgsConstructor
@ToString
@EqualsAndHashCode
public class NewsletterIssueNumber implements Comparable<NewsletterIssueNumber>, Serializable {

    private final Long value;

    public static NewsletterIssueNumber of(Long value) {
        return new NewsletterIssueNumber(value);
    }

    public static NewsletterIssueNumber next(NewsletterIssueNumber newsletterIssueNumber) {
        return new NewsletterIssueNumber(newsletterIssueNumber.asLong() + 1L);
    }

    public static NewsletterIssueNumber previous(NewsletterIssueNumber newsletterIssueNumber) {
        return new NewsletterIssueNumber(newsletterIssueNumber.asLong() - 1L);
    }

    public Long asLong() {
        return value;
    }

    @Override
    public int compareTo(NewsletterIssueNumber anotherIssue) {
        return asLong().compareTo(anotherIssue.asLong());
    }

}
