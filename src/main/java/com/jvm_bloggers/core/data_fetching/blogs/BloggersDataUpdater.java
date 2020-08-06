package com.jvm_bloggers.core.data_fetching.blogs;

import com.jvm_bloggers.core.data_fetching.blogs.json_data.BloggerEntry;
import com.jvm_bloggers.core.data_fetching.blogs.json_data.BloggersData;
import com.jvm_bloggers.core.rss.SyndFeedProducer;
import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.utils.NowProvider;
import io.vavr.control.Option;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL;

@Component
@Slf4j
@RequiredArgsConstructor
public class BloggersDataUpdater {

    private final BlogRepository blogRepository;
    private final NowProvider nowProvider;
    private final SyndFeedProducer syndFeedFactory;
    private final BloggerChangedVerifier bloggerChangedVerifier;
    private final ApplicationEventPublisher eventPublisher;

    public UpdateStatistic updateData(BloggersData data) {
        return data.getBloggers()
            .parallelStream()
            .filter(BloggerEntry::hasRss)
            .map(this::updateSingleEntry)
            .collect(UpdateStatistic.collector());
    }

    private UpdateStatus updateSingleEntry(BloggerEntry bloggerEntry) {
        return blogRepository
            .findByBookmarkableId(bloggerEntry.getBookmarkableId())
            .map(bloggerWithSameId ->
                updateBloggerIfThereAreAnyChanges(bloggerEntry, bloggerWithSameId))
            .getOrElse(() -> createNewBlogger(bloggerEntry));
    }

    private UpdateStatus updateBloggerIfThereAreAnyChanges(BloggerEntry bloggerEntry,
                                                           Blog existingBlogger) {
        Option<String> validBlogUrl = extractValidBlogUrlFromFeed(bloggerEntry.getRss());
        validBlogUrl.forEach(bloggerEntry::setUrl);

        if (bloggerChangedVerifier.pendingChanges(existingBlogger, bloggerEntry)) {
            existingBlogger.setBookmarkableId(bloggerEntry.getBookmarkableId());
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

    private Option<String> extractValidBlogUrlFromFeed(String rss) {
        return syndFeedFactory.validUrlFromRss(rss);
    }

    private UpdateStatus createNewBlogger(BloggerEntry bloggerEntry) {
        return extractValidBlogUrlFromFeed(bloggerEntry.getRss())
        .map(blogUrl -> createNewBlogWithExtractedUrl(bloggerEntry, blogUrl))
        .map(this::publishNewBlogCreated)
        .getOrElse(() -> {
            log.warn("No url found for blog {}, Skipping", bloggerEntry.getRss());
            return UpdateStatus.INVALID;
        });
    }

    private UpdateStatus publishNewBlogCreated(Blog newBlog) {
        eventPublisher.publishEvent(new NewBlogAdded(newBlog));
        return UpdateStatus.CREATED;
    }

    private Blog createNewBlogWithExtractedUrl(BloggerEntry bloggerEntry, String blogUrl) {
        return blogRepository.save(
            Blog.builder()
            .bookmarkableId(bloggerEntry.getBookmarkableId())
            .author(bloggerEntry.getName())
            .rss(bloggerEntry.getRss())
            .twitter(bloggerEntry.getTwitter())
            .url(blogUrl)
            .dateAdded(nowProvider.now())
            .blogType(bloggerEntry.getBlogType())
            .active(true)
            .moderationRequired(isModerationRequired(bloggerEntry))
            .build());
    }

    private boolean isModerationRequired(BloggerEntry bloggerEntry) {
        return bloggerEntry.getBlogType() != PERSONAL
            || bloggerEntry.getRss() != null && bloggerEntry.getRss().contains("//medium.com");
    }
}
