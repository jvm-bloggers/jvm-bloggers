package pl.tomaszdziurko.jvm_bloggers.blog_posts.domain;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface BlogPostRepository extends JpaRepository<BlogPost, Long> {

    Optional<BlogPost> findByUrl(String url);

    List<BlogPost> findByPublishedDateAfterAndApprovedTrueOrderByPublishedDateAsc(
        LocalDateTime publishedDate);

    List<BlogPost> findByApprovedTrueOrderByPublishedDateDesc(Pageable page);

    @Query("from BlogPost bp order by "
           + "case when bp.approved is null then 0 else 1 end, "
           + "bp.publishedDate desc")
    List<BlogPost> findLatestPosts(Pageable page);

    int countByPublishedDateAfter(LocalDateTime publishedDate);

    int countByApprovedIsNull();

}
