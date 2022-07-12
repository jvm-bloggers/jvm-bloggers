package com.jvm_bloggers.core.data_fetching.blog_posts;

import com.jvm_bloggers.core.rss.SyndFeedProducer;
import com.jvm_bloggers.entities.blog.Blog;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@RequiredArgsConstructor
@Component
@Slf4j
public class SingleRssChecker {

    private final SyndFeedProducer syndFeedFactory;
    private final BlogPostService blogPostService;

    public void checkForNewEntries(Blog person) {
        log.info("Checking for new entries at {} by {}.", person.getRss(), person.getAuthor());
        syndFeedFactory.createFor(person.getRss()).forEach(feed -> {
                    log.info("For author {} found {} entries in rss feed", person.getAuthor(), feed.getEntries().size());
                    feed.getEntries().forEach(post -> {
                        blogPostService.addOrUpdate(new RssEntryWithAuthor(person, post));
                    });
                }

        );
    }
}
