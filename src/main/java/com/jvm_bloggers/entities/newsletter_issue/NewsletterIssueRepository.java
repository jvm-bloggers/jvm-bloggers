package com.jvm_bloggers.entities.newsletter_issue;

import io.vavr.collection.Seq;

import io.vavr.control.Option;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface NewsletterIssueRepository extends JpaRepository<NewsletterIssue, Long> {

    Option<NewsletterIssue> findByIssueNumber(Long issueNumber);

    Option<NewsletterIssue> findFirstByOrderByPublishedDateDesc();

    Seq<NewsletterIssue> findByOrderByPublishedDateDesc(Pageable request);

    Seq<NewsletterIssue> findAllByOrderByPublishedDateDesc();

    boolean existsByIssueNumber(Long issueNumber);

}
