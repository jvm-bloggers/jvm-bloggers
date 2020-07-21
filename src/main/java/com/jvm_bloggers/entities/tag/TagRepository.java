package com.jvm_bloggers.entities.tag;

import io.vavr.control.Option;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TagRepository extends JpaRepository<Tag, Long> {

    Option<Tag> findByValue(String value);

}
