package com.jvm_bloggers.entities.click;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;

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

    private static final int REFERER_MAX_LENGTH = 256;
    private static final int USER_AGENT_MAX_LENGTH = 256;

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
