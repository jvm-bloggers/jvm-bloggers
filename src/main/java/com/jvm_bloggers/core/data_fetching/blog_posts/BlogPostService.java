package com.jvm_bloggers.core.data_fetching.blog_posts;


import static com.google.common.base.MoreObjects.firstNonNull;
import static com.jvm_bloggers.core.utils.Validators.isUrlValid;
import static org.apache.commons.lang3.StringUtils.abbreviate;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.entities.tag.Tag;
import com.jvm_bloggers.entities.tag.TagRepository;
import com.jvm_bloggers.utils.DateTimeUtilities;
import com.rometools.rome.feed.synd.SyndCategory;
import com.rometools.rome.feed.synd.SyndContent;
import com.rometools.rome.feed.synd.SyndEntry;
import io.vavr.control.Option;
import java.util.Date;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Slf4j
public class BlogPostService {

    private static final String HTTPS_PREFIX = "https";
    private static final String HTTP_PREFIX = "http";

    private final BlogPostRepository blogPostRepository;
    private final BlogPostFactory blogPostFactory;
    private final TagRepository tagRepository;

    @Transactional
    public Option<BlogPost> addOrUpdate(RssEntryWithAuthor rssEntry) {
        String blogPostLink = rssEntry.getRssEntry().getLink();
        if (isUrlValid(blogPostLink)) {
            BlogPost blogPost = blogPostRepository
                    .findByUrlEndingWith(removeProtocolFrom(blogPostLink))
                    .getOrElse(() -> createBlogPost(rssEntry));
            updateDescription(blogPost, rssEntry.getRssEntry().getDescription());
            updateTags(blogPost, rssEntry.getRssEntry());
            blogPostRepository.save(blogPost);
            return Option.of(blogPost);
        } else {
            log.warn(
                    "Detected blog post with invalid link {}. Skipping DB operation",
                    blogPostLink
            );
            return Option.none();
        }
    }

    private String removeProtocolFrom(String link) {
        return link
                .replaceAll("^" + HTTPS_PREFIX, "")
                .replaceAll("^" + HTTP_PREFIX, "");
    }

    private BlogPost createBlogPost(RssEntryWithAuthor rssEntry) {
        SyndEntry postInRss = rssEntry.getRssEntry();
        Date dateToStore = firstNonNull(postInRss.getPublishedDate(), postInRss.getUpdatedDate());
        log.info("Creating new post '{}' by {}", postInRss.getTitle(),
                rssEntry.getBlog().getAuthor());
        return blogPostFactory.create(
                postInRss.getTitle(),
                postInRss.getLink(),
                DateTimeUtilities.toLocalDateTime(dateToStore),
                rssEntry.getBlog());
    }

    private void updateDescription(BlogPost blogPost, SyndContent descriptionContent) {
        if (descriptionContent != null) {
            String description = abbreviate(descriptionContent.getValue(), BlogPost.MAX_DESCRIPTION_LENGTH);
            blogPost.setDescription(description);
        }
    }

    private void updateTags(BlogPost blogPost, SyndEntry syndEntry) {
        final Set<String> lowerCaseTags = mapSyndEntryCategoriesToLowerCaseStrings(syndEntry);
        if (shouldUpdateTags(blogPost, lowerCaseTags)) {
            Set<Tag> tagEntities = new HashSet<>();
            for (String stringTag : lowerCaseTags) {
                Tag tagEntity = tagRepository.findByValue(stringTag)
                        .getOrElse(() -> new Tag(stringTag));
                tagEntities.add(tagEntity);
            }
            blogPost.setTags(tagEntities);
            log.info("new tags for {}, : {}", blogPost.getUrl(), tagEntities);
        }
    }

    private boolean shouldUpdateTags(BlogPost blogPost, Set<String> syndEntryTags) {
        var entityTags = blogPost.getTags()
                .stream()
                .map(Tag::getValue)
                .collect(Collectors.toSet());

        return !Objects.equals(syndEntryTags, entityTags);
    }

    private Set<String> mapSyndEntryCategoriesToLowerCaseStrings(SyndEntry syndEntry) {
        return syndEntry.getCategories()
                .stream()
                .map(SyndCategory::getName)
                .filter(StringUtils::isNotBlank)
                .map(String::toLowerCase)
                .collect(Collectors.toUnmodifiableSet());
    }

}
