package pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain;


import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;

import java.time.LocalDate;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import static lombok.AccessLevel.PRIVATE;

@Entity
@Table(name = "newsletter_issue")
@NoArgsConstructor(access = PRIVATE)
@Getter
@ToString
public class NewsletterIssue {

    @Id
    @GeneratedValue(generator = "NEWSLETTER_ISSUE_SEQ", strategy = GenerationType.SEQUENCE)
    @SequenceGenerator(name = "NEWSLETTER_ISSUE_SEQ", sequenceName = "NEWSLETTER_ISSUE_SEQ",
        allocationSize = 1)
    @Column(name = "ID")
    private Long id;

    @Column(name = "ISSUE_NUMBER", nullable = false, updatable = false)
    private Long issueNumber;

    @Column(name = "PUBLISHED_DATE", updatable = false)
    private LocalDate publishedDate;

    @Column(name = "HEADING", length = 4000, updatable = false)
    private String heading;

    @OneToMany
    @JoinColumn(name = "NEWSLETTER_ISSUE_ID", updatable = false)
    private List<BlogPost> blogPosts;

    @OneToMany
    @JoinColumn(name = "NEWSLETTER_ISSUE_ID", updatable = false)
    private List<Blog> newBlogs;

    @Column(name = "VARIA", length = 4000, updatable = false)
    private String varia;

    public NewsletterIssue(
        Long issueNumber,
        LocalDate publishedDate,
        List<Blog> newBlogs,
        List<BlogPost> blogPosts,
        String heading,
        String varia) {

        this.issueNumber = issueNumber;
        this.publishedDate = publishedDate;
        this.newBlogs = newBlogs;
        this.blogPosts = blogPosts;
        this.heading = heading;
        this.varia = varia;
    }
}
