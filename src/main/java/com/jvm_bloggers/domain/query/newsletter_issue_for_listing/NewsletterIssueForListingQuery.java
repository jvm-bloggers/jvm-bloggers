package com.jvm_bloggers.domain.query.newsletter_issue_for_listing;

import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository;
import javaslang.collection.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.function.Function;

import static com.jvm_bloggers.domain.query.NewsletterIssueNumber.of;

@Service
@Transactional(readOnly = true)
public class NewsletterIssueForListingQuery {

    private final NewsletterIssueRepository repository;

    @Autowired
    public NewsletterIssueForListingQuery(NewsletterIssueRepository repository) {
        this.repository = repository;
    }

    public List<NewsletterIssueForListing> findLatestIssues(int count) {
        List<NewsletterIssue> latestIssues = List.ofAll(repository.findByOrderByPublishedDateDesc(
            new PageRequest(0, count)
        ));
        return latestIssues.map(convertToDomainObject());
    }

    public List<NewsletterIssueForListing> findAllByOrderByPublishedDateDesc() {
        return List.ofAll(repository.findAllByOrderByPublishedDateDesc()).map(convertToDomainObject());
    }

    private Function<NewsletterIssue, NewsletterIssueForListing> convertToDomainObject() {
        return issue -> new NewsletterIssueForListing(
            of(issue.getIssueNumber()),
            issue.getPublishedDate()
        );
    }

}
