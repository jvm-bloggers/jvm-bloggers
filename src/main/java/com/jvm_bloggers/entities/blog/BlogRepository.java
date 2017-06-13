package com.jvm_bloggers.entities.blog;

import io.vavr.control.Option;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
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
}
