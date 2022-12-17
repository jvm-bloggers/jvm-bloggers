package com.jvm_bloggers.entities.tag;

import com.jvm_bloggers.entities.blog_post.BlogPost;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;
import org.hibernate.search.mapper.pojo.mapping.definition.annotation.FullTextField;
import org.hibernate.search.mapper.pojo.mapping.definition.annotation.Indexed;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToMany;

import static lombok.AccessLevel.PRIVATE;

@Entity
@Getter
@Setter
@NoArgsConstructor(access = PRIVATE)
@ToString(of = "value")
@Indexed(index = "idx_tag")
public class Tag {

    @Id
    @GenericGenerator(
        name = "TAG_SEQ",
        strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
        parameters = {
            @Parameter(name = "sequence_name", value = "TAG_SEQ"),
            @Parameter(name = "initial_value", value = "1"),
            @Parameter(name = "increment_size", value = "1")
        }
    )
    @GeneratedValue(generator = "PERSON_SEQ")
    private Long id;

    @Column(nullable = false, unique = true, updatable = false)
    @FullTextField(name = "tag")
    private String value;

    @ManyToMany(mappedBy = "tags")
    private Set<BlogPost> posts = new HashSet<>();

    public Tag(String value) {
        this.value = value.toLowerCase();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Tag tag1 = (Tag) o;
        return value.equals(tag1.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

}
