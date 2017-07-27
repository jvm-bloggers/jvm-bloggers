package com.jvm_bloggers.entities.email;

import io.vavr.control.Option;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface EmailRepository extends JpaRepository<Email, Long> {

    Option<Email> findFirstBySentDateNull();

}
