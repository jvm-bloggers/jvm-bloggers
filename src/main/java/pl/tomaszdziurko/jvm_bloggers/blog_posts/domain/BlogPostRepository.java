package pl.tomaszdziurko.jvm_bloggers.blog_posts.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface BlogPostRepository extends JpaRepository<BlogPost, Long> {

    Optional<BlogPost> findByUrl(String url);

    List<BlogPost> findByPublishedDateAfterOrderByPublishedDateAsc(LocalDateTime publishedDate);
}
