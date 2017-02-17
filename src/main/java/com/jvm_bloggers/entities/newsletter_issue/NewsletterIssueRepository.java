package com.jvm_bloggers.entities.newsletter_issue;

import com.jvm_bloggers.core.newsletter_issues.domain.NewsletterIssueBaseData;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface NewsletterIssueRepository extends JpaRepository<NewsletterIssue, Long> {

    Optional<NewsletterIssue> findByIssueNumber(Long issueNumber);

    Optional<NewsletterIssue> findFirstByOrderByPublishedDateDesc();

    List<NewsletterIssueBaseData> findTop5ByOrderByPublishedDateDesc();
    
    List<NewsletterIssue> findAllByOrderByPublishedDateDesc();
}
