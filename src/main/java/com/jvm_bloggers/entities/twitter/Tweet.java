package com.jvm_bloggers.entities.twitter;

import lombok.AccessLevel;
import lombok.Getter;
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
@Table(name = "tweet")
@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Tweet {

    @Id
    @GenericGenerator(
        name = "TWEET_SEQ",
        strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
        parameters = {
            @Parameter(name = "sequence_name", value = "TWEET_SEQ"),
            @Parameter(name = "initial_value", value = "1"),
            @Parameter(name = "increment_size", value = "1")
        }
    )
    @GeneratedValue(generator = "TWEET_SEQ")
    private Long id;

    @NonNull
    @Column(name = "content", nullable = false)
    private String content;

    @NonNull
    @Column(name = "posting_date", nullable = false)
    private LocalDateTime postingDate;

    @Column(name = "sent")
    private boolean sent;

    public Tweet(@NonNull String content, @NonNull LocalDateTime postingDate) {
        this.content = content;
        this.postingDate = postingDate;
    }

    public void markAsSent() {
        this.sent = true;
    }

}
