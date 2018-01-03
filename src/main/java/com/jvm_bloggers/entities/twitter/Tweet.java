package com.jvm_bloggers.entities.twitter;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;

import java.time.LocalDateTime;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "tweet")
@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Tweet {

    @Id
    @GeneratedValue(generator = "TWEET_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "TWEET_SEQ", sequenceName = "TWEET_SEQ",
        allocationSize = 1)
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
