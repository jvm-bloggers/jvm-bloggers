package com.jvm_bloggers.frontend.newsletter_issue;

import com.jvm_bloggers.core.newsletter_issues.domain.NewsletterIssue;
import com.jvm_bloggers.core.newsletter_issues.domain.NewsletterIssueRepository;

import lombok.AllArgsConstructor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


import java.util.Optional;

@Component
@AllArgsConstructor(onConstructor = @__(@Autowired))
public class NewsletterIssueDtoService {

    private final NewsletterIssueRepository newsletterIssueRepository;
    private final NewsletterIssueDtoBuilder newsletterIssueDtoBuilder;

    public Optional<NewsletterIssueDto> getLatestIssue() {
        Optional<NewsletterIssue> latestIssue =
            newsletterIssueRepository.findFirstByOrderByPublishedDateDesc();
        return latestIssue.map(newsletterIssueDtoBuilder::build);
    }

    public Optional<NewsletterIssueDto> findByIssueNumber(long issueNumber) {
        Optional<NewsletterIssue> issue =
            newsletterIssueRepository.findByIssueNumber(issueNumber);
        return issue.map(newsletterIssueDtoBuilder::build);
    }
}
