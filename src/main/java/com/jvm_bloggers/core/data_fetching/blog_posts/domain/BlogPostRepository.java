package com.jvm_bloggers.core.data_fetching.blog_posts.domain;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@Repository
public interface BlogPostRepository extends JpaRepository<BlogPost, Long> {

    Optional<BlogPost> findByUrl(String url);

    Optional<BlogPost> findByUid(String uid);

    List<BlogPost> findByPublishedDateAfterAndApprovedTrueOrderByPublishedDateAsc(
        LocalDateTime publishedDate);

    List<BlogPost> findByApprovedTrueAndBlogAuthorNotInOrderByPublishedDateDesc(
        Pageable page, Set<String> excludedAuthors);

    @Query("from BlogPost bp order by "
           + "case when bp.approved is null then 0 else 1 end, "
           + "bp.publishedDate desc")
    List<BlogPost> findLatestPosts(Pageable page);

    int countByPublishedDateAfter(LocalDateTime publishedDate);

    int countByApprovedIsNull();

}
