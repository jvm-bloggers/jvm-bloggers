package com.jvm_bloggers.entities.top_posts_summary;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import java.time.LocalDateTime;
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.OrderBy;
import javax.persistence.Table;

import static javax.persistence.CascadeType.ALL;

@Entity
@Getter
@Table(name = "top_posts_summary")
@NoArgsConstructor
public class TopPostsSummary {

    @Id
    @GenericGenerator(
        name = "TOP_POSTS_SUMMARY_SEQ",
        strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
        parameters = {
            @Parameter(name = "sequence_name", value = "TOP_POSTS_SUMMARY_SEQ"),
            @Parameter(name = "initial_value", value = "1"),
            @Parameter(name = "increment_size", value = "1")
        }
    )
    @GeneratedValue(generator = "TOP_POSTS_SUMMARY_SEQ")
    private Long id;

    @OneToMany(cascade = ALL, orphanRemoval = true, mappedBy = "topPostsSummary")
    @OrderBy("position asc")
    private List<PopularPersonalPost> popularPersonalPosts;

    @OneToMany(cascade = ALL, orphanRemoval = true, mappedBy = "topPostsSummary")
    @OrderBy("position asc")
    private List<PopularCompanyPost> popularCompanyPosts;

    @Column(name = "YEAR", nullable = false)
    private Integer year;

    @Column(name = "MONTH", nullable = false)
    private Integer month;

    @Column(name = "CREATED", nullable = false)
    private LocalDateTime created;

    public TopPostsSummary(Integer year, Integer month, LocalDateTime created,
                           List<PopularPersonalPost> popularPersonalPosts,
                           List<PopularCompanyPost> popularCompanyPosts
                           ) {
        this.year = year;
        this.month = month;
        this.created = created;
        this.popularPersonalPosts = new ArrayList<>();
        this.popularCompanyPosts = new ArrayList<>();

        popularPersonalPosts.forEach(this::add);
        popularCompanyPosts.forEach(this::add);
    }

    public List<PopularPersonalPost> add(PopularPersonalPost post) {
        post.setTopPostsSummary(this);
        popularPersonalPosts.add(post);
        return popularPersonalPosts;
    }

    public List<PopularCompanyPost> add(PopularCompanyPost post) {
        post.setTopPostsSummary(this);
        popularCompanyPosts.add(post);
        return popularCompanyPosts;
    }

    public YearMonth getYearMonth() {
        return YearMonth.of(year, month);
    }

}
