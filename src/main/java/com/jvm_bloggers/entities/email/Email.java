package com.jvm_bloggers.entities.email;

import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import java.time.LocalDateTime;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "email")
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Email {

    @Id
    @GenericGenerator(
        name = "EMAIL_SEQ",
        strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
        parameters = {
            @Parameter(name = "sequence_name", value = "EMAIL_SEQ"),
            @Parameter(name = "initial_value", value = "1"),
            @Parameter(name = "increment_size", value = "1")
        }
    )
    @GeneratedValue(generator = "EMAIL_SEQ")
    private Long id;

    @NonNull
    @Column(name = "from_address", nullable = false, length = 250)
    private String fromAddress;

    @NonNull
    @Column(name = "to_address", nullable = false, length = 250)
    private String toAddress;

    @NonNull
    @Column(name = "title", nullable = false, length = 500)
    private String title;

    @NonNull
    @Column(name = "content", nullable = false)
    private String content;

    @NonNull
    @Column(name = "sent_date", nullable = false)
    private LocalDateTime sentDate;

    public Email(String fromAddress, String to, String title, String content) {
        this.fromAddress = fromAddress;
        this.toAddress = to;
        this.title = title;
        this.content = content;
    }
}
