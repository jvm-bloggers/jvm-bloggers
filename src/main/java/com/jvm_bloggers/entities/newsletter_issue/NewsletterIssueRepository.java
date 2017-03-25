package com.jvm_bloggers.entities.newsletter_issue;

import javaslang.collection.List;
import javaslang.control.Option;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface NewsletterIssueRepository extends JpaRepository<NewsletterIssue, Long> {

    Option<NewsletterIssue> findByIssueNumber(Long issueNumber);

    Option<NewsletterIssue> findFirstByOrderByPublishedDateDesc();

    List<NewsletterIssue> findByOrderByPublishedDateDesc(PageRequest request);
    
    List<NewsletterIssue> findAllByOrderByPublishedDateDesc();

}
