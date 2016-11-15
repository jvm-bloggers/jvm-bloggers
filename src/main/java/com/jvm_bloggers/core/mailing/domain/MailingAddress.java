package com.jvm_bloggers.core.mailing.domain;

import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

@Entity
@Table(name = "mailing_address")
@Data
@NoArgsConstructor(force = true, access = AccessLevel.PRIVATE)
@RequiredArgsConstructor
public class MailingAddress implements Serializable {

    @Id
    @GeneratedValue(generator = "MAILING_ADDRESS_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "MAILING_ADDRESS_SEQ", sequenceName = "MAILING_ADDRESS_SEQ",
            allocationSize = 1)
    private Long id;

    @NonNull
    @NotNull
    @Column(name = "address", unique = true, nullable = false, length = 250)
    private String address;

}
