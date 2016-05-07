package pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface NewsletterIssueRepository extends JpaRepository<NewsletterIssue, Long> {

    Optional<NewsletterIssue> findByIssueNumber(Long issueNumber);

    NewsletterIssue findFirstByOrderByPublishedDateDesc();

    List<NewsletterIssue> findTop5ByOrderByPublishedDateDesc();

}
