package pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue;

import lombok.AllArgsConstructor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue;
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssueRepository;

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
