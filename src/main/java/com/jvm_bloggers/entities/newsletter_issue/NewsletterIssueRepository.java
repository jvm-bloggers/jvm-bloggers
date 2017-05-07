package com.jvm_bloggers.entities.newsletter_issue;

import javaslang.collection.List;
import javaslang.control.Option;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface NewsletterIssueRepository extends JpaRepository<NewsletterIssue, Long> {

    Option<NewsletterIssue> findByIssueNumber(Long issueNumber);

    Option<NewsletterIssue> findFirstByOrderByPublishedDateDesc();

    //TODO: Fix it once https://jira.spring.io/browse/DATACMNS-1005 is released (at least spring-data-commons 1.13.2)
    java.util.List<NewsletterIssue> findByOrderByPublishedDateDesc(Pageable request);

    List<NewsletterIssue> findAllByOrderByPublishedDateDesc();

    @Query("select case when count(*) > 0 then true else false end "
        + "from NewsletterIssue where issueNumber = :issueNumber")
    boolean issueNumberExist(@Param("issueNumber") Long issueNumber);

}
