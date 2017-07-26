package com.jvm_bloggers.entities.click;

import com.jvm_bloggers.entities.blog.BlogType;
import io.vavr.collection.List;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;

@Repository
public interface ClickRepository extends JpaRepository<Click, Long> {

    @Query(value = "select new com.jvm_bloggers.entities.click.PostIdWithCount(c.blogPost.id, "
        + "count(c) as counter) from click c "
        + "where "
        + "    c.clickDate >= :fromValue and "
        + "    c.clickDate <= :toValue and "
        + "    c.blogPost.blog.blogType= :typeValue "
        + "group by c.blogPost.id "
        + "order by counter desc, c.blogPost.id desc ")
    List<PostIdWithCount> calculateMostPopularBlogPosts(
        @Param("fromValue") LocalDateTime from,
        @Param("toValue") LocalDateTime to,
        @Param("typeValue") BlogType personal,
        Pageable page
    );

}
