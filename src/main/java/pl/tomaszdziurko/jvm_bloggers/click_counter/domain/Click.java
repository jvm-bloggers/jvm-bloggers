package pl.tomaszdziurko.jvm_bloggers.click_counter.domain;

import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.NonNull;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;

import java.time.LocalDateTime;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Table
@Entity(name = "click")
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Click {

    @Id
    @GeneratedValue(generator = "CLICK_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "CLICK_SEQ", sequenceName = "CLICK_SEQ")
    @Column(name = "ID")
    private Long id;

    @NonNull
    @OneToOne(cascade = CascadeType.MERGE, optional = false)
    @JoinColumn(name = "BLOG_POST_ID", nullable = false)
    private BlogPost blogPost;

    @NonNull
    @Column(name = "CLICK_DATE", nullable = false)
    private LocalDateTime clickDate;

    public Click(BlogPost blogPost, LocalDateTime clickDate) {
        this.blogPost = blogPost;
        this.clickDate = clickDate;
    }
}
