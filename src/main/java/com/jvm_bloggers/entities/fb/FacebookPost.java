package com.jvm_bloggers.entities.fb;

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
@Table(name = "fb_post")
@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class FacebookPost {

    @Id
    @GenericGenerator(
        name = "FB_POST_SEQ",
        strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
        parameters = {
            @Parameter(name = "sequence_name", value = "FB_POST_SEQ"),
            @Parameter(name = "initial_value", value = "1"),
            @Parameter(name = "increment_size", value = "1")
        }
    )
    @GeneratedValue(generator = "FB_POST_SEQ")
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
