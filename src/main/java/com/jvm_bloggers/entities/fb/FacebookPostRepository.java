package com.jvm_bloggers.entities.fb;

import io.vavr.control.Option;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface FacebookPostRepository extends JpaRepository<FacebookPost, Long> {

    Option<FacebookPost> findFirstBySentDateNull();

}
