package pl.tomaszdziurko.jvm_bloggers.blogs;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;
import pl.tomaszdziurko.jvm_bloggers.blogs.json_data.BloggerEntry;
import pl.tomaszdziurko.jvm_bloggers.blogs.json_data.BloggersData;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
@Slf4j
public class BloggersDataUpdater {

    private final BlogRepository blogRepository;
    private final NowProvider nowProvider;

    @Autowired
    public BloggersDataUpdater(BlogRepository blogRepository, NowProvider nowProvider) {
        this.blogRepository = blogRepository;
        this.nowProvider = nowProvider;
    }

    public void updateData(BloggersData data) {
        List<BloggerEntry> entries = data.getBloggers().stream().filter(entry -> entry.getRss().length() > 0).collect(Collectors.toList());
        UpdateSummary updateSummary = new UpdateSummary(entries.size());
        entries.stream().forEach(entry -> updateSingleEntry(entry, updateSummary));
        log.info("Bloggers Data updated: totalRecordsInFile={}, updatedRecords={}, newRecords={}", updateSummary.numberOfEntries,
            updateSummary.updatedEntries, updateSummary.createdEntries);
    }

    protected void updateSingleEntry(BloggerEntry bloggerEntry, UpdateSummary updateSummary) {
        Optional<Blog> existingBloggerByJsonId = blogRepository.findByJsonId(bloggerEntry.getJsonId());

        if (existingBloggerByJsonId.isPresent()) {
            Blog bloggerWithSameJsonId = existingBloggerByJsonId.get();
            updateBloggerIfThereAreSomeChanges(bloggerEntry, updateSummary, bloggerWithSameJsonId);
        } else {
            Blog newBlog = new Blog(bloggerEntry.getJsonId(), bloggerEntry.getName(),
                StringUtils.lowerCase(bloggerEntry.getRss()), bloggerEntry.getTwitter(), nowProvider.now(), bloggerEntry.getBlogType());
            blogRepository.save(newBlog);
            updateSummary.recordCreated();
        }
    }

    private void updateBloggerIfThereAreSomeChanges(BloggerEntry bloggerEntry, UpdateSummary updateSummary, Blog existingBlogger) {
        if (!isEqual(existingBlogger, bloggerEntry)) {
            existingBlogger.setJsonId(bloggerEntry.getJsonId());
            existingBlogger.setAuthor(bloggerEntry.getName());
            existingBlogger.setTwitter(bloggerEntry.getTwitter());
            existingBlogger.setRss(bloggerEntry.getRss());
            existingBlogger.setBlogType(bloggerEntry.getBlogType());
            blogRepository.save(existingBlogger);
            updateSummary.recordUpdated();
        }
    }

    protected boolean isEqual(Blog blog, BloggerEntry bloggerEntry) {
        return Objects.equals(blog.getAuthor(), bloggerEntry.getName())
            && Objects.equals(blog.getJsonId(), bloggerEntry.getJsonId())
            && StringUtils.equalsIgnoreCase(blog.getRss(), bloggerEntry.getRss())
            && Objects.equals(blog.getBlogType(), bloggerEntry.getBlogType())
            && Objects.equals(blog.getTwitter(), bloggerEntry.getTwitter());
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
