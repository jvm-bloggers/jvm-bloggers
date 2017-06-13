package com.jvm_bloggers.entities.blog_post;


import com.google.common.annotations.VisibleForTesting;
import com.jvm_bloggers.entities.blog.Blog;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.NonNull;

import org.apache.commons.lang3.RandomStringUtils;

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "blog_post")
@Data
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@Builder
@EntityListeners(BlogPostEntityListener.class)
public class BlogPost {

    public static final int MAX_DESCRIPTION_LENGTH = 4096;
    public static final int UID_LENGTH = 7;

    @Column(name = "UID", unique = true, nullable = false)
    private final String uid = generateRandomUid();

    @Id
    @GeneratedValue(generator = "BLOG_POST_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "BLOG_POST_SEQ", sequenceName = "BLOG_POST_SEQ", allocationSize = 1)
    @Column(name = "ID")
    private Long id;

    @NonNull
    @Column(name = "TITLE", nullable = false, length = 250)
    private String title;

    @Column(name = "DESCRIPTION", length = MAX_DESCRIPTION_LENGTH)
    private String description;

    @NonNull
    @Column(name = "URL", unique = true, nullable = false, length = 500)
    private String url;

    @NonNull
    @Column(name = "PUBLISHED_DATE", nullable = false)
    private LocalDateTime publishedDate;

    @Column(name = "APPROVED")
    private Boolean approved;

    @Column(name = "APPROVED_DATE")
    private LocalDateTime approvedDate;

    @NonNull
    @ManyToOne
    @JoinColumn(name = "BLOG_ID", nullable = false)
    private Blog blog;

    public boolean isApproved() {
        return Boolean.TRUE.equals(approved);
    }

    public boolean isRejected() {
        return Boolean.FALSE.equals(approved);
    }

    @VisibleForTesting
    boolean isModerated() {
        return approved != null;
    }

    public String getApprovalState() {
        if (approved == null) {
            return " -- ";
        } else if (approved) {
            return "Approved";
        } else {
            return "Rejected";
        }
    }

    public boolean isGoingInNewsletter(final LocalDateTime lastPublicationDate) {
        return isApproved() && approvedDate.isAfter(lastPublicationDate);
    }

    private static String generateRandomUid() {
        return RandomStringUtils.randomAlphanumeric(UID_LENGTH);
    }

    public void approve(LocalDateTime approvedDate) {
        this.approvedDate = approvedDate;
        approved = Boolean.TRUE;
    }

    public void reject() {
        approved = Boolean.FALSE;
    }
}
