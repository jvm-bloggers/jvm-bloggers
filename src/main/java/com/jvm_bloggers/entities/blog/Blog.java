package com.jvm_bloggers.entities.blog;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import java.time.LocalDateTime;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "blog")
@Data
@Builder
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Blog {

    @Id
    @GenericGenerator(
        name = "PERSON_SEQ",
        strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
        parameters = {
            @Parameter(name = "sequence_name", value = "PERSON_SEQ"),
            @Parameter(name = "initial_value", value = "1"),
            @Parameter(name = "increment_size", value = "1")
        }
    )
    @GeneratedValue(generator = "PERSON_SEQ")
    @Column(name = "ID")
    private Long id;

    @NonNull
    @Column(nullable = false, name = "BOOKMARKABLE_ID", length = 30, unique = true)
    private String bookmarkableId;

    @NonNull
    @Column(name = "AUTHOR", nullable = false, length = 250)
    private String author;

    @NonNull
    @Column(name = "RSS", unique = true, nullable = false, length = 250)
    private String rss;

    @NonNull
    @Column(name = "URL", unique = true, nullable = false, length = 250)
    private String url;

    @Column(name = "TWITTER", length = 100)
    private String twitter;

    @NonNull
    @Column(name = "DATE_ADDED", nullable = false)
    private LocalDateTime dateAdded;

    @NonNull
    @Column(name = "BLOG_TYPE", nullable = false)
    @Enumerated(value = EnumType.STRING)
    private BlogType blogType;

    @Column(name = "ACTIVE")
    private boolean active;

    @Column(name = "MODERATION_REQUIRED", nullable = false)
    private Boolean moderationRequired;

    public Boolean isModerationRequired() {
        return moderationRequired;
    }

    public Boolean getInitialApprovedValue() {
        return moderationRequired ? null : true;
    }

    public boolean isPersonal() {
        return BlogType.PERSONAL == blogType;
    }

    public boolean isCompany() {
        return BlogType.COMPANY == blogType;
    }

    public String getStatus() {
        return active ? "Active" : "Deactivated";
    }
}
