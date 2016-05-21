package pl.tomaszdziurko.jvm_bloggers.mailing.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface EmailRepository extends JpaRepository<Email, Long> {

    Optional<Email> findFirstBySentDateNull();

}
