package pl.tomaszdziurko.jvm_bloggers.blogs;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;
import pl.tomaszdziurko.jvm_bloggers.blogs.json_data.BloggerEntry;
import pl.tomaszdziurko.jvm_bloggers.blogs.json_data.BloggersData;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;
import pl.tomaszdziurko.jvm_bloggers.utils.SyndFeedProducer;

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
        entries.stream().forEach(entry -> updateSingleEntry(entry, updateSummary));
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
        Blog newBlog = Blog.builder()
            .jsonId(bloggerEntry.getJsonId())
            .author(bloggerEntry.getName())
            .rss(bloggerEntry.getRss())
            .url(syndFeedFactory.validUrlFromRss(
                bloggerEntry.getRss()).orElse(null)
            )
            .twitter(bloggerEntry.getTwitter())
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
