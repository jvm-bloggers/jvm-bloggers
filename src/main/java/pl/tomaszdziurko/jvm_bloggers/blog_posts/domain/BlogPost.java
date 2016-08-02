package pl.tomaszdziurko.jvm_bloggers.blog_posts.domain;


import com.google.common.annotations.VisibleForTesting;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.NonNull;

import org.apache.commons.lang3.RandomStringUtils;

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;

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
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
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
        return publishedDate.isAfter(lastPublicationDate);
    }

    public static BlogPostBuilder builder() {
        return new BlogPostBuilder();
    }

    private static String generateRandomUid() {
        return RandomStringUtils.randomAlphanumeric(UID_LENGTH);
    }

    public static class BlogPostBuilder {
        private String uid  = generateRandomUid();
        private Long id;
        private String title;
        private String description;
        private String url;
        private LocalDateTime publishedDate;
        private Boolean approved;
        private Blog blog;

        BlogPostBuilder() {
        }

        public BlogPost.BlogPostBuilder id(Long id) {
            this.id = id;
            return this;
        }

        public BlogPost.BlogPostBuilder title(String title) {
            this.title = title;
            return this;
        }

        public BlogPost.BlogPostBuilder description(String description) {
            this.description = description;
            return this;
        }

        public BlogPost.BlogPostBuilder url(String url) {
            this.url = url;
            return this;
        }

        public BlogPost.BlogPostBuilder publishedDate(LocalDateTime publishedDate) {
            this.publishedDate = publishedDate;
            return this;
        }

        public BlogPost.BlogPostBuilder approved(Boolean approved) {
            this.approved = approved;
            return this;
        }

        public BlogPost.BlogPostBuilder blog(Blog blog) {
            this.blog = blog;
            return this;
        }

        public BlogPost build() {
            return new BlogPost(id, title, description, url, publishedDate, approved, blog);
        }

        public String toString() {
            return "pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost.BlogPostBuilder(uid="
                + this.uid + ", id=" + this.id + ", title=" + this.title + ", description="
                + this.description + ", url=" + this.url + ", publishedDate=" + this.publishedDate
                + ", approved=" + this.approved + ", blog=" + this.blog + ")";
        }
    }

}
