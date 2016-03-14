package pl.tomaszdziurko.jvm_bloggers.blogs.domain;

import lombok.Data;
import lombok.NoArgsConstructor;

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
@NoArgsConstructor
public class Blog {

    @Id
    @GeneratedValue(generator = "PERSON_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "PERSON_SEQ", sequenceName = "PERSON_SEQ", allocationSize = 1)
    @Column(name = "ID")
    private Long id;

    @Column(name = "JSON_ID")
    private Long jsonId;

    @Column(name = "AUTHOR", nullable = false, length = 250)
    private String author;

    @Column(name = "RSS", unique = true, nullable = false, length = 250)
    private String rss;

    @Column(name = "TWITTER", nullable = true, length = 100)
    private String twitter;

    @Column(name = "DATE_ADDED", nullable = false)
    private LocalDateTime dateAdded;

    @Column(name = "BLOG_TYPE", nullable = false)
    @Enumerated(value = EnumType.STRING)
    private BlogType blogType;

    public Blog(Long jsonId, String author, String rss, String twitter, LocalDateTime dateAdded,
                BlogType blogType) {
        this.jsonId = jsonId;
        this.author = author;
        this.rss = rss;
        this.twitter = twitter;
        this.dateAdded = dateAdded;
        this.blogType = blogType;
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

}
