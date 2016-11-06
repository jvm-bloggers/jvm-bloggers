package com.jvm_bloggers.core.data_fetching.blogs;

import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog;
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogRepository;
import com.jvm_bloggers.core.data_fetching.blogs.json_data.BloggerEntry;
import com.jvm_bloggers.core.data_fetching.blogs.json_data.BloggersData;
import com.jvm_bloggers.core.rss.SyndFeedProducer;
import com.jvm_bloggers.utils.NowProvider;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
@Slf4j
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class BloggersDataUpdater {

    private final BlogRepository blogRepository;
    private final NowProvider nowProvider;
    private final SyndFeedProducer syndFeedFactory;

    public void updateData(BloggersData data) {
        List<BloggerEntry> entries = data.getBloggers().stream()
            .filter(entry -> entry.getRss().length() > 0)
            .collect(Collectors.toList());
        UpdateSummary updateSummary = new UpdateSummary(entries.size());
        entries.parallelStream().forEach(entry -> updateSingleEntry(entry, updateSummary));
        log.info("Bloggers Data updated: totalRecordsInFile={}, updatedRecords={}, newRecords={}",
            updateSummary.numberOfEntries,
            updateSummary.updatedEntries, updateSummary.createdEntries);
    }

    protected void updateSingleEntry(BloggerEntry bloggerEntry, UpdateSummary updateSummary) {
        Optional<Blog> existingBloggerByJsonId =
            blogRepository.findByJsonId(bloggerEntry.getJsonId());

        if (existingBloggerByJsonId.isPresent()) {
            Blog bloggerWithSameJsonId = existingBloggerByJsonId.get();
            updateBloggerIfThereAreSomeChanges(bloggerEntry, updateSummary, bloggerWithSameJsonId);
        } else {
            createNewBlogger(bloggerEntry, updateSummary);
        }
    }

    private void updateBloggerIfThereAreSomeChanges(BloggerEntry bloggerEntry,
                                                    UpdateSummary updateSummary,
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
            updateSummary.recordUpdated();
        }
    }

    private Optional<String> extractValidBlogUrlFromFeed(String rss) {
        return syndFeedFactory.validUrlFromRss(rss);
    }

    private void createNewBlogger(BloggerEntry bloggerEntry,
                                  UpdateSummary updateSummary) {
        Optional<String> validBlogUrl = extractValidBlogUrlFromFeed(bloggerEntry.getRss());
        if (!validBlogUrl.isPresent()) {
            log.warn("No url found for blog {}, Skipping", bloggerEntry.getRss());
            return;
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
        updateSummary.recordCreated();
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

    @Getter
    public static class UpdateSummary {

        private int numberOfEntries;
        private int updatedEntries;
        private int createdEntries;

        public UpdateSummary(int numberOfEntries) {
            this.numberOfEntries = numberOfEntries;
        }

        public void recordUpdated() {
            updatedEntries++;
        }

        public void recordCreated() {
            createdEntries++;
        }
    }

}
