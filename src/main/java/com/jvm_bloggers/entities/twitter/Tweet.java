package com.jvm_bloggers.entities.twitter;

import lombok.AccessLevel;
import lombok.Data;
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
@Data
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
    @Column(name = "sent_date", nullable = false)
    private LocalDateTime sentDate;

    public Tweet(String content) {
        this.content = content;
    }
}
