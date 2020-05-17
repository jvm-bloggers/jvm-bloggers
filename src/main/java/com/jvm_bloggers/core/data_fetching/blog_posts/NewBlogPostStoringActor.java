package com.jvm_bloggers.core.data_fetching.blog_posts;

import akka.actor.AbstractActor;
import akka.actor.Props;

import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.entities.tag.Tag;
import com.jvm_bloggers.entities.tag.TagRepository;
import com.jvm_bloggers.utils.DateTimeUtilities;
import com.rometools.rome.feed.synd.SyndCategory;
import com.rometools.rome.feed.synd.SyndContent;
import com.rometools.rome.feed.synd.SyndEntry;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;

import java.util.Date;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static com.google.common.base.MoreObjects.firstNonNull;
import static com.jvm_bloggers.core.utils.Validators.isUrlValid;
import static org.apache.commons.lang3.StringUtils.abbreviate;

@Slf4j
@RequiredArgsConstructor
public class NewBlogPostStoringActor extends AbstractActor {

    private static final String HTTPS_PREFIX = "https";
    private static final String HTTP_PREFIX = "http";

    private final BlogPostRepository blogPostRepository;
    private final BlogPostFactory blogPostFactory;
    private final TagRepository tagRepository;

    @Override
    public Receive createReceive() {
        return receiveBuilder().match(RssEntryWithAuthor.class,
            rssEntry -> {
                String blogPostLink = rssEntry.getRssEntry().getLink();
                if (isUrlValid(blogPostLink)) {
                    BlogPost blogPost = blogPostRepository
                            .findByUrlEndingWith(removeProtocolFrom(blogPostLink))
                            .getOrElse(() -> createBlogPost(rssEntry));
                    updateDescription(blogPost, rssEntry.getRssEntry().getDescription());
                    updateTags(blogPost, rssEntry.getRssEntry());
                    blogPostRepository.save(blogPost);
                } else {
                    log.info(
                            "Detected blog post with invalid link {}. Skipping DB operation",
                        blogPostLink
                    );
                }
            }).build();
    }

    public static Props props(BlogPostRepository blogPostRepository,
                              BlogPostFactory blogPostFactory,
                              TagRepository tagRepository) {
        return Props.create(
            NewBlogPostStoringActor.class,
            () -> new NewBlogPostStoringActor(blogPostRepository, blogPostFactory, tagRepository)
        );
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
            String description = descriptionContent.getValue();
            description = abbreviate(description, BlogPost.MAX_DESCRIPTION_LENGTH);
            blogPost.setDescription(description);
        }
    }

    private void updateTags(BlogPost blogPost, SyndEntry syndEntry) {
        final Set<String> lowerCaseTags = mapSyndEntryCategoriesToLowerCaseStrings(syndEntry);
        if (shouldUpdateTags(blogPost, lowerCaseTags)) {
            Set<Tag> tagEntities = new HashSet<>();
            for (String stringTag: lowerCaseTags) {
                Tag tagEntity = tagRepository.findByTag(stringTag)
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
            .map(Tag::getTag)
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
