package com.jvm_bloggers.entities.top_posts_summary;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import static lombok.AccessLevel.PRIVATE;

@Entity
@Getter
@Table(name = "popular_company_post")
@NoArgsConstructor(access = PRIVATE)
public class PopularCompanyPost extends BasePopularBlogPost {

    @Id
    @GenericGenerator(
        name = "POPULAR_COMPANY_POST_SEQ",
        strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
        parameters = {
            @Parameter(name = "sequence_name", value = "POPULAR_COMPANY_POST_SEQ"),
            @Parameter(name = "initial_value", value = "1"),
            @Parameter(name = "increment_size", value = "1")
        }
    )
    @GeneratedValue(generator = "POPULAR_COMPANY_POST_SEQ")
    private Long id;

    public PopularCompanyPost(BlogPost blogPost, Long position, Long numberOfClicks) {
        super(blogPost, position, numberOfClicks);
    }

}

