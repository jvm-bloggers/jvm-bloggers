package pl.tomaszdziurko.jvm_bloggers.mailing.domain;


import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "mailing_address")
@Data
@NoArgsConstructor
public class MailingAddress {

    @Id
    @GeneratedValue(generator = "MAILING_ADDRESS_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "MAILING_ADDRESS_SEQ", sequenceName = "MAILING_ADDRESS_SEQ",
        allocationSize = 1)
    private Long id;

    @Column(name = "address", unique = true, nullable = false, length = 250)
    private String address;

    public MailingAddress(String address) {
        this.address = address;
    }
}
