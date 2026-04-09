package com.jvm_bloggers.core.data_fetching.blog_posts;

import com.jvm_bloggers.core.rss.SyndFeedProducer;
import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog.BlogRepository;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

import javax.transaction.Transactional;

@RequiredArgsConstructor
@Component
@Slf4j
public class SingleRssChecker {

    private final SyndFeedProducer syndFeedFactory;
    private final BlogPostService blogPostService;
    private final BlogRepository blogRepository;


    @Transactional
    public void checkForNewEntries(Blog person) {
        log.info("Checking for new entries at {} by {}.", person.getRss(), person.getAuthor());
        syndFeedFactory.createFor(person.getRss()).forEach(feed -> {
                log.info("For author {} found {} entries in rss feed", person.getAuthor(),
                    feed.getEntries().size());
                feed.getEntries().forEach(post -> {
                    blogPostService.addOrUpdate(new RssEntryWithAuthor(person, post));
                });

                blogRepository.updateDateLastFetched(LocalDateTime.now(), person.getId());
            }
        );
    }
}
