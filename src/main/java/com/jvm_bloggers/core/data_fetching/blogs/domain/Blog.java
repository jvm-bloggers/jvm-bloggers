package com.jvm_bloggers.core.data_fetching.blogs.domain;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.NonNull;

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "blog")
@Data
@Builder
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Blog {

    @Id
    @GeneratedValue(generator = "PERSON_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "PERSON_SEQ", sequenceName = "PERSON_SEQ", allocationSize = 1)
    @Column(name = "ID")
    private Long id;

    @NonNull
    @Column(nullable = false, name = "JSON_ID")
    private Long jsonId;

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

    public boolean isPersonal() {
        return BlogType.PERSONAL == blogType;
    }

    public Boolean getDefaultApprovedValue() {
        return isPersonal() ? Boolean.TRUE : null; 
    }

    public String getStatus() {
        return active ? "Active" : "Deactivated";
    }
}
