package com.jvm_bloggers.entities.blog;

import com.jvm_bloggers.entities.blog.projections.BlogStatisticsProjection;

import javaslang.control.Option;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface BlogRepository extends JpaRepository<Blog, Long> {

    Option<Blog> findByJsonId(Long jsonId);

    List<Blog> findByDateAddedAfter(LocalDateTime lastNewsletterSent);

    Page<Blog> findAllByOrderByAuthorAsc(Pageable pageable);

    @Query("from Blog b where b.active = true")
    List<Blog> findAllActiveBlogs();

    @Query(""
        + "SELECT b.id as id, "
        + "b.url as url, "
        + "b.author as author, "
        + "b.twitter as twitter, "
        + "SUM(CASE WHEN bp.publishedDate >= :first THEN 1 ELSE 0 END) as firstCount, "
        + "SUM(CASE WHEN bp.publishedDate >= :second THEN 1 ELSE 0 END) as secondCount "
        + "FROM BlogPost bp JOIN bp.blog b "
        + "WHERE b.blogType = :blogType "
        + "GROUP BY b.id, b.url, b.author, b.twitter "
        + "ORDER BY firstCount DESC, secondCount DESC")
    javaslang.collection.List<BlogStatisticsProjection> findBlogStatistics(
        @Param("first") LocalDateTime first,
        @Param("second") LocalDateTime second,
        @Param("blogType") BlogType blogType,
        Pageable pageable);

    long countByBlogType(BlogType blogType);
}
