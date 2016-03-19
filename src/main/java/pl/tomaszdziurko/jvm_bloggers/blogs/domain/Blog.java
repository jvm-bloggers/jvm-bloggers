package pl.tomaszdziurko.jvm_bloggers.blogs.domain;

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
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

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

    @Column(nullable = false, name = "JSON_ID")
    private Long jsonId;

    @Column(name = "AUTHOR", nullable = false, length = 250)
    private String author;

    @Column(name = "RSS", unique = true, nullable = false, length = 250)
    private String rss;

    @Column(name = "TWITTER", length = 100)
    private String twitter;

    @Column(name = "DATE_ADDED", nullable = false)
    private LocalDateTime dateAdded;

    @Column(name = "BLOG_TYPE", nullable = false)
    @Enumerated(value = EnumType.STRING)
    private BlogType blogType;

    @Column(name = "ACTIVE", nullable = false)
    private boolean active;

    public Blog(Long jsonId, String author, String rss, String twitter, LocalDateTime dateAdded, BlogType blogType) {
        this.jsonId = jsonId;
        this.author = author;
        this.rss = rss;
        this.twitter = twitter;
        this.dateAdded = dateAdded;
        this.blogType = blogType;
        this.active = true;
    }

    public boolean isPersonal() {
        return BlogType.PERSONAL == blogType;
    }

    public Boolean getDefaultApprovedValue() {
        if (isPersonal()) {
            return Boolean.TRUE;
        } else {
            return null;
        }
    }

    public String getStatus() {
        return active ? "Active" : "Deactivated";
    }

    public String getTwitterUrl() {
        return getTwitter() != null ? "https://twitter.com/" + getTwitter() : null;
    }
}
