package com.jvm_bloggers.entities.blog_post;

import static org.apache.commons.text.CharacterPredicates.DIGITS;
import static org.apache.commons.text.CharacterPredicates.LETTERS;

import com.google.common.annotations.VisibleForTesting;
import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.tag.Tag;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.apache.commons.text.RandomStringGenerator;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;
import org.hibernate.search.annotations.Analyze;
import org.hibernate.search.annotations.Field;
import org.hibernate.search.annotations.Indexed;
import org.hibernate.search.annotations.IndexedEmbedded;
import org.hibernate.search.annotations.SortableField;

@Entity
@Table(name = "blog_post")
@Data
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@Builder
@EntityListeners(BlogPostEntityListener.class)
@Indexed(index = "idx_blog_post")
public class BlogPost {

    public static final int MAX_DESCRIPTION_LENGTH = 4096;
    public static final int UID_LENGTH = 7;

    @Column(name = "UID", unique = true, nullable = false)
    private final String uid = generateRandomUid();

    @Id
    @GenericGenerator(
        name = "BLOG_POST_SEQ",
        strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
        parameters = {
            @Parameter(name = "sequence_name", value = "BLOG_POST_SEQ"),
            @Parameter(name = "initial_value", value = "1"),
            @Parameter(name = "increment_size", value = "1")
        }
    )
    @GeneratedValue(generator = "BLOG_POST_SEQ")
    @Column(name = "ID")
    private Long id;

    @NonNull
    @Column(name = "TITLE", nullable = false, length = 250)
    @Field(analyze = Analyze.YES)
    private String title;

    @Column(name = "DESCRIPTION", length = MAX_DESCRIPTION_LENGTH)
    @Field
    private String description;

    @NonNull
    @Column(name = "URL", unique = true, nullable = false, length = 500)
    @Field
    private String url;

    @NonNull
    @Column(name = "PUBLISHED_DATE", nullable = false)
    @Field
    @SortableField
    private LocalDateTime publishedDate;

    @Column(name = "APPROVED")
    @Field
    private Boolean approved;

    @Column(name = "APPROVED_DATE")
    @Field
    private LocalDateTime approvedDate;

    @NonNull
    @ManyToOne
    @JoinColumn(name = "BLOG_ID", nullable = false)
    @IndexedEmbedded
    private Blog blog;

    @ManyToMany(fetch = FetchType.LAZY, cascade = {CascadeType.ALL})
    @JoinTable(name = "POST_TAG", joinColumns = @JoinColumn(name = "POST_ID"),
        inverseJoinColumns = @JoinColumn(name = "TAG_ID"))
    @Builder.Default
    @IndexedEmbedded
    private Set<Tag> tags = new HashSet<>();

    public boolean isApproved() {
        return Boolean.TRUE.equals(approved);
    }

    public boolean isRejected() {
        return Boolean.FALSE.equals(approved);
    }

    @VisibleForTesting
    boolean isModerated() {
        return approved != null;
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

    public void setTags(Set<Tag> tags) {
        removeNoLongerPresentTags(tags);
        tags.forEach(t -> {
            this.tags.add(t);
            t.getPosts().add(this);

        });
    }

    private void removeNoLongerPresentTags(Set<Tag> tags) {
        var tagsToRemove = getTags().stream().filter(t -> !tags.contains(t)).collect(Collectors.toSet());
        tagsToRemove.forEach(toRemove -> {
            toRemove.getPosts().remove(this);
            getTags().remove(toRemove);
        });
    }

    public boolean isGoingInNewsletter(final LocalDateTime lastPublicationDate) {
        return isApproved() && approvedDate.isAfter(lastPublicationDate);
    }

    private static String generateRandomUid() {
        return new RandomStringGenerator.Builder()
            .withinRange('0', 'z')
            .filteredBy(LETTERS, DIGITS)
            .build()
            .generate(UID_LENGTH);
    }

    public void approve(LocalDateTime approvedDate) {
        this.approvedDate = approvedDate;
        approved = Boolean.TRUE;
    }

    public void reject() {
        approved = Boolean.FALSE;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof BlogPost)) {
            return false;
        }
        BlogPost blogPost = (BlogPost) o;
        return Objects.equals(getUid(), blogPost.getUid());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getUid());
    }

}
