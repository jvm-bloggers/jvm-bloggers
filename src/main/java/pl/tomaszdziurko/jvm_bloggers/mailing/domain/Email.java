package pl.tomaszdziurko.jvm_bloggers.mailing.domain;


import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "email")
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Email {

    @Id
    @GeneratedValue(generator = "EMAIL_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "EMAIL_SEQ", sequenceName = "EMAIL_SEQ",
        allocationSize = 1)
    private Long id;

    @Column(name = "from_address", nullable = false, length = 250)
    private String fromAddress;

    @Column(name = "to_address", nullable = false, length = 250)
    private String toAddress;

    @Column(name = "title", nullable = false, length = 500)
    private String title;

    @Column(name = "content", nullable = false)
    private String content;

    @Column(name = "sent_date", nullable = false)
    private LocalDateTime sentDate;

    public Email(String fromAddress, String to, String title, String content) {
        this.fromAddress = fromAddress;
        this.toAddress = to;
        this.title = title;
        this.content = content;
    }
}
