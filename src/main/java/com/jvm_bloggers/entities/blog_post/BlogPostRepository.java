package com.jvm_bloggers.entities.blog_post;

import com.jvm_bloggers.entities.blog.BlogType;
import io.vavr.control.Option;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface BlogPostRepository extends JpaRepository<BlogPost, Long> {

    Option<BlogPost> findByUrlEndingWith(String urlWithoutProtocol);

    Option<BlogPost> findByUid(String uid);

    List<BlogPost> findByApprovedDateAfterAndApprovedTrueOrderByApprovedDateAsc(
            LocalDateTime publishedDate);

    List<BlogPost> findByApprovedTrueAndBlogAuthorNotInOrderByApprovedDateDesc(
            Pageable page, Set<String> excludedAuthors);

    @Query("from BlogPost bp order by "
            + "case when bp.approved is null then 0 else 1 end, "
            + "bp.publishedDate desc")
    List<BlogPost> findLatestPosts(Pageable page);

    int countByPublishedDateAfter(LocalDateTime publishedDate);

    int countByApprovedIsNull();

    List<BlogPost> findByBlogIdAndApprovedTrueOrderByPublishedDateDesc(Long blogId, Pageable page);

    List<BlogPost> findByBlogIdOrderByPublishedDateDesc(Long blogId, Pageable page);

    int countByBlogId(Long blogId);

    @Query("FROM BlogPost bp JOIN bp.blog b WHERE b.blogType = :blogType")
    List<BlogPost> findBlogPostsOfType(@Param("blogType") BlogType blogType, Pageable page);

    @Query("FROM BlogPost bp JOIN bp.blog b " +
            "WHERE b.blogType = :blogType " +
            "AND bp.approved is null " +
            "ORDER BY bp.publishedDate DESC")
    List<BlogPost> findUnapprovedPostsByBlogType(@Param("blogType") BlogType blogType, Pageable page);

}
