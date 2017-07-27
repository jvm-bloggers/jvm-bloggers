package com.jvm_bloggers.entities.fb;

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
@Table(name = "fb_post")
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class FacebookPost {

    @Id
    @GeneratedValue(generator = "FB_POST_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "FB_POST_SEQ", sequenceName = "FB_POST_SEQ",
        allocationSize = 1)
    private Long id;

    @NonNull
    @Column(name = "link", nullable = false)
    private String link;

    @NonNull
    @Column(name = "message", nullable = false)
    private String message;

    @NonNull
    @Column(name = "sent_date", nullable = false)
    private LocalDateTime sentDate;

    public FacebookPost(String link, String message) {
        this.link = link;
        this.message = message;
    }
}
