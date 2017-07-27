package com.jvm_bloggers.entities.twitter;

import io.vavr.control.Option;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TweetRepository extends JpaRepository<Tweet, Long> {

    Option<Tweet> findFirstBySentDateNull();

}
