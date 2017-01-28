package com.jvm_bloggers.core.data_fetching.blogs;

import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog;
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogRepository;
import com.jvm_bloggers.core.data_fetching.blogs.json_data.BloggerEntry;
import com.jvm_bloggers.core.data_fetching.blogs.json_data.BloggersData;
import com.jvm_bloggers.core.rss.SyndFeedProducer;
import com.jvm_bloggers.utils.NowProvider;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Objects;
import java.util.Optional;

@Component
@Slf4j
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class BloggersDataUpdater {

    private final BlogRepository blogRepository;
    private final NowProvider nowProvider;
    private final SyndFeedProducer syndFeedFactory;

    public UpdateStatistic updateData(BloggersData data) {
        return data.getBloggers()
            .parallelStream()
            .filter(BloggerEntry::hasRss)
            .map(this::updateSingleEntry)
            .collect(UpdateStatistic.collector());
    }

    private UpdateStatus updateSingleEntry(BloggerEntry bloggerEntry) {
        return blogRepository
            .findByJsonId(bloggerEntry.getJsonId())
            .map(bloggerWithSameId ->
                updateBloggerIfThereAreAnyChanges(bloggerEntry, bloggerWithSameId))
            .orElseGet(() -> createNewBlogger(bloggerEntry));
    }

    private UpdateStatus updateBloggerIfThereAreAnyChanges(BloggerEntry bloggerEntry,
                                                           Blog existingBlogger) {
        Optional<String> validBlogUrl = extractValidBlogUrlFromFeed(bloggerEntry.getRss());
        validBlogUrl.ifPresent(bloggerEntry::setUrl);

        if (somethingChangedInBloggerData(existingBlogger, bloggerEntry)) {
            existingBlogger.setJsonId(bloggerEntry.getJsonId());
            existingBlogger.setAuthor(bloggerEntry.getName());
            existingBlogger.setTwitter(bloggerEntry.getTwitter());
            existingBlogger.setRss(bloggerEntry.getRss());
            existingBlogger.setBlogType(bloggerEntry.getBlogType());
            if (StringUtils.isNotBlank(bloggerEntry.getUrl())) {
                existingBlogger.setUrl(bloggerEntry.getUrl());
            }
            blogRepository.save(existingBlogger);
            return UpdateStatus.UPDATED;
        } else {
            return UpdateStatus.NOT_CHANGED;
        }
    }

    private Optional<String> extractValidBlogUrlFromFeed(String rss) {
        return syndFeedFactory.validUrlFromRss(rss);
    }

    private UpdateStatus createNewBlogger(BloggerEntry bloggerEntry) {
        Optional<String> validBlogUrl = extractValidBlogUrlFromFeed(bloggerEntry.getRss());
        if (!validBlogUrl.isPresent()) {
            log.warn("No url found for blog {}, Skipping", bloggerEntry.getRss());
            return UpdateStatus.INVALID;
        }
        validBlogUrl.ifPresent(bloggerEntry::setUrl);

        Blog newBlog = Blog.builder()
            .jsonId(bloggerEntry.getJsonId())
            .author(bloggerEntry.getName())
            .rss(bloggerEntry.getRss())
            .url(syndFeedFactory.validUrlFromRss(
                bloggerEntry.getRss()).orElse(null)
            )
            .twitter(bloggerEntry.getTwitter())
            .url(bloggerEntry.getUrl())
            .dateAdded(nowProvider.now())
            .blogType(bloggerEntry.getBlogType())
            .active(true)
            .build();
        blogRepository.save(newBlog);
        return UpdateStatus.CREATED;
    }

    protected boolean somethingChangedInBloggerData(Blog blog, BloggerEntry bloggerEntry) {
        return !Objects.equals(blog.getAuthor(), bloggerEntry.getName())
            || !Objects.equals(blog.getJsonId(), bloggerEntry.getJsonId())
            || !StringUtils.equalsIgnoreCase(blog.getRss(), bloggerEntry.getRss())
            || !Objects.equals(blog.getBlogType(), bloggerEntry.getBlogType())
            || !Objects.equals(blog.getTwitter(), bloggerEntry.getTwitter())
            || urlFromRssIsValidAndDifferentThanExistingOne(blog, bloggerEntry);
    }

    private boolean urlFromRssIsValidAndDifferentThanExistingOne(Blog blog,
                                                                 BloggerEntry bloggerEntry) {
        return StringUtils.isNotBlank(bloggerEntry.getUrl())
            && !StringUtils.equalsIgnoreCase(blog.getUrl(), bloggerEntry.getUrl());
    }

}
