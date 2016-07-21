package pl.tomaszdziurko.jvm_bloggers.mailing.domain;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MailingAddressRepository extends JpaRepository<MailingAddress, Long> {

    Page<MailingAddress> findAllByOrderByAddressAsc(Pageable pageable);

}
