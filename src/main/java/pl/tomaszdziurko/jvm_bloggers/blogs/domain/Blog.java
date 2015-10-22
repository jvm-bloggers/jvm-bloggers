package pl.tomaszdziurko.jvm_bloggers.blogs.domain;

import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import java.time.LocalDateTime;

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

    public Blog(Long jsonId, String author, String rss, String twitter, LocalDateTime dateAdded) {
        this.jsonId = jsonId;
        this.author = author;
        this.rss = rss;
        this.twitter = twitter;
        this.dateAdded = dateAdded;
    }

}
