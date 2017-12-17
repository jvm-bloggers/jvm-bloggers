package com.jvm_bloggers.entities.fb;

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
@Table(name = "fb_post")
@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class FacebookPost {

    @Id
    @GeneratedValue(generator = "FB_POST_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "FB_POST_SEQ", sequenceName = "FB_POST_SEQ",
        allocationSize = 1)
    private Long id;

    @Column(name = "link", nullable = false)
    private String link;

    @Column(name = "message", nullable = false)
    private String message;

    @Column(name = "posting_date")
    private LocalDateTime postingDate;

    @Column(name = "sent")
    private boolean sent;

    public FacebookPost(
        @NonNull String link,
        @NonNull String message,
        @NonNull LocalDateTime postingDate
    ) {
        this.link = link;
        this.message = message;
        this.postingDate = postingDate;
    }

    public void markAsSent() {
        this.sent = true;
    }

}
