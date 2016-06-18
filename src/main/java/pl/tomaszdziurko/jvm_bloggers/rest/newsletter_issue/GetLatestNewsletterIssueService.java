package pl.tomaszdziurko.jvm_bloggers.rest.newsletter_issue;

import lombok.AllArgsConstructor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue;
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssueRepository;

import java.util.Optional;

@Component
@AllArgsConstructor(onConstructor = @__(@Autowired))
public class GetLatestNewsletterIssueService {

    private final NewsletterIssueRepository newsletterIssueRepository;

    Optional<NewsletterIssueDto> getLatestIssue() {
        Optional<NewsletterIssue> latestIssue =
            newsletterIssueRepository.findFirstByOrderByPublishedDateDesc();
        return latestIssue.map(NewsletterIssueDto::fromIssue);
    }
}
