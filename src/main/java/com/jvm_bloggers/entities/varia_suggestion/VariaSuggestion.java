package com.jvm_bloggers.entities.varia_suggestion;

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

@Table
@Entity(name = "varia_suggestion")
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class VariaSuggestion {

    private static final int AUTHOR_MAX_LENGTH = 250;
    private static final int REASON_MAX_LENGTH = 2048;
    private static final int URL_MAX_LENGTH = 500;

    @Id
    @GeneratedValue(generator = "VARIA_SUGGESTION_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "VARIA_SUGGESTION_SEQ", sequenceName = "VARIA_SUGGESTION_SEQ")
    @Column(name = "ID")
    private Long id;

    @NonNull
    @Column(name = "URL", nullable = false, length = URL_MAX_LENGTH)
    private String url;

    @NonNull
    @Column(name = "REASON", nullable = false, length = REASON_MAX_LENGTH)
    private String reason;

    @Column(name = "AUTHOR", length = AUTHOR_MAX_LENGTH)
    private String author;

    @NonNull
    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @Column(name = "READ")
    private Boolean read;

    public VariaSuggestion(String url, String reason, String author) {
        this.url = url;
        this.reason = reason;
        this.author = author;
        this.createDate = LocalDateTime.now();
    }
}
