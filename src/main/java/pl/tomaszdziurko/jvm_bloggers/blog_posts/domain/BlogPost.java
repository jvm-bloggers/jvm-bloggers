package pl.tomaszdziurko.jvm_bloggers.blog_posts.domain;


import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "blog_post")
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@Builder
public class BlogPost {

    @Id
    @GeneratedValue(generator = "BLOG_POST_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "BLOG_POST_SEQ", sequenceName = "BLOG_POST_SEQ", allocationSize = 1)
    @Column(name = "ID")
    private Long id;

    @Column(name = "UID", unique = true, nullable = false)
    private final String uid = UUID.randomUUID().toString();

    @Column(name = "TITLE", nullable = false, length = 250)
    private String title;

    @Column(name = "URL", unique = true, nullable = false, length = 500)
    private String url;

    @Column(name = "PUBLISHED_DATE", nullable = false)
    private LocalDateTime publishedDate;

    @Column(name = "APPROVED", nullable = true)
    private Boolean approved;

    @ManyToOne
    @JoinColumn(name = "BLOG_ID", nullable = false)
    private Blog blog;

    public boolean isApproved() {
        return Boolean.TRUE.equals(approved);
    }

    public boolean isRejected() {
        return Boolean.FALSE.equals(approved);
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
}
