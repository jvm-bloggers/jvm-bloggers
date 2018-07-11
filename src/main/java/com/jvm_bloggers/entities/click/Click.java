package com.jvm_bloggers.entities.click;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import java.time.LocalDateTime;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Table
@Entity(name = "click")
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Click {

    private static final int REFERER_MAX_LENGTH = 256;
    private static final int USER_AGENT_MAX_LENGTH = 256;

    @Id
    @GenericGenerator(
        name = "CLICK_SEQ",
        strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
        parameters = {
            @Parameter(name = "sequence_name", value = "CLICK_SEQ"),
            @Parameter(name = "initial_value", value = "1"),
            @Parameter(name = "increment_size", value = "1")
        }
    )
    @GeneratedValue(generator = "CLICK_SEQ")
    @Column(name = "ID")
    private Long id;

    @NonNull
    @OneToOne(cascade = CascadeType.MERGE, optional = false)
    @JoinColumn(name = "BLOG_POST_ID", nullable = false)
    private BlogPost blogPost;

    @NonNull
    @Column(name = "CLICK_DATE", nullable = false)
    private LocalDateTime clickDate;

    @NonNull
    @Column(name = "IP_ADDRESS", nullable = false, length = 20)
    private String ipAddress;

    @NonNull
    @Column(name = "REFERER", nullable = false, length = REFERER_MAX_LENGTH)
    private String referer;

    @Column(name = "USER_AGENT", nullable = false, length = USER_AGENT_MAX_LENGTH)
    private String userAgent;

    public Click(
        BlogPost blogPost,
        LocalDateTime clickDate,
        String ipAddress,
        String referer,
        String userAgent
    ) {
        this.blogPost = blogPost;
        this.clickDate = clickDate;
        this.ipAddress = ipAddress;
        this.referer = StringUtils.abbreviate(referer, REFERER_MAX_LENGTH);
        this.userAgent = StringUtils.abbreviate(userAgent, USER_AGENT_MAX_LENGTH);
    }
}
