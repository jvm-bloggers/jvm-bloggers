package com.jvm_bloggers.core.mailing.domain;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface MailingAddressRepository extends JpaRepository<MailingAddress, Long> {

    Page<MailingAddress> findAllByOrderByAddressAsc(Pageable pageable);

    @Query("select case when count(*) > 0 then true else false end " +
            "from MailingAddress " +
            "where address = :address and (:id is null or id != :id)")
    boolean addressExistsIgnoringId(@Param("address") String address, @Param("id") Long id);

}
