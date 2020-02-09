package com.jvm_bloggers.core.rss;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableSet;
import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator;
import com.jvm_bloggers.core.utils.Validators;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.utils.DateTimeUtilities;
import com.jvm_bloggers.utils.NowProvider;
import com.rometools.rome.feed.synd.SyndContentImpl;
import com.rometools.rome.feed.synd.SyndEntry;
import com.rometools.rome.feed.synd.SyndEntryImpl;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.feed.synd.SyndFeedImpl;
import com.rometools.rome.feed.synd.SyndLink;
import com.rometools.rome.feed.synd.SyndLinkImpl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.util.StopWatch;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

@Service
@CacheConfig(cacheNames = AggregatedRssFeedProducer.RSS_CACHE)
@RequiredArgsConstructor
@Slf4j
public class AggregatedRssFeedProducer {

    public static final String RSS_CACHE = "Aggregated RSS feed cache";
    @VisibleForTesting
    static final String FEED_DESCRIPTION =
        "JVM Bloggers aggregated feed. You can customize your rss results by using parameters "
        + "`limit` and 'excludedAuthors` (comma delimited names) parameters. "
        + "Example: http://jvm-bloggers.com/pl/rss?limit=5&excludedAuthors=Tomasz Dziurko Adam Warski";

    @VisibleForTesting
    static final String FEED_TITLE = "JVM Bloggers";
    @VisibleForTesting
    static final String FEED_TYPE = "atom_1.0";
    @VisibleForTesting
    static final Set<String> INCLUDE_ALL_AUTHORS_SET = ImmutableSet.of(StringUtils.EMPTY);

    public static final String SELF_REL = "self";
    private final BlogPostRepository blogPostRepository;
    private final NowProvider nowProvider;
    private final LinkGenerator linkGenerator;

    /**
     * Generates aggregated RSS feed for all or given <tt>limit</tt> of approved blog posts.
     *
     * @param feedUrl Value of the <tt>{@literal <link rel=self/>}</tt> element
     *     of the generated feed. It identifies the URL of the web site associated
     *     with the generated feed. Cannot be <tt>null</tt> nor empty.
     *
     * @param limit Upper limit of RSS entries count in the generated feed. If equal or less
     *     than zero then all approved blog posts will be generated.
     *
     * @param excludedAuthors RSS entries for given authors will be excluded
     *     from the generated feed (may be <code>null</code>)
     *
     * @return Aggregated RSS feed for approved blog posts ordered by publication date
     */
    @Cacheable
    public SyndFeed getRss(String feedUrl, int limit, Set<String> excludedAuthors) {

        Preconditions.checkArgument(isNotBlank(feedUrl), "feedUrl parameter cannot be blank");

        StopWatch stopWatch = null;
        if (log.isDebugEnabled()) {
            stopWatch = new StopWatch();
            log.debug("Building aggregated RSS feed...");
            stopWatch.start();
        }

        final Pageable pageRequest = PageRequest.of(0, limit > 0 ? limit : Integer.MAX_VALUE);
        if (CollectionUtils.isEmpty(excludedAuthors)) {
            excludedAuthors = INCLUDE_ALL_AUTHORS_SET;
        }
        final List<BlogPost> approvedPosts = blogPostRepository
            .findByApprovedTrueAndBlogAuthorNotInOrderByApprovedDateDesc(pageRequest, excludedAuthors);
        final List<SyndEntry> feedItems = approvedPosts.stream()
            .filter(it -> Validators.isUrlValid(it.getUrl()))
            .map(this::toRssEntry)
            .collect(Collectors.toList());
        final SyndFeed feed = buildFeed(feedItems, feedUrl);

        if (log.isDebugEnabled()) {
            stopWatch.stop();
            log.debug("Total {} feed entries produced in {}ms", feedItems.size(),
                stopWatch.getTotalTimeMillis());
        }
        return feed;
    }

    private SyndEntry toRssEntry(BlogPost post) {
        final SyndEntry rssEntry = new SyndEntryImpl();
        rssEntry.setTitle(post.getTitle());
        rssEntry.setLink(linkGenerator.generateRedirectLinkFor(post.getUid()));
        rssEntry.setAuthor(post.getBlog().getAuthor());
        rssEntry.setPublishedDate(DateTimeUtilities.toDate(post.getPublishedDate()));
        rssEntry.setUri(post.getUid());

        final String description = post.getDescription();
        if (isNotBlank(description)) {
            final SyndContentImpl descriptionContent = new SyndContentImpl();
            descriptionContent.setValue(description);
            rssEntry.setDescription(descriptionContent);
        }

        return rssEntry;
    }

    private SyndFeed buildFeed(final List<SyndEntry> feedItems, String requestedUrlString) {
        final SyndLink feedLink = new SyndLinkImpl();
        feedLink.setRel(SELF_REL);
        feedLink.setHref(requestedUrlString);
        final SyndFeed feed = new SyndFeedImpl();
        feed.setUri(FEED_TITLE);
        feed.setTitle(FEED_TITLE);
        feed.setFeedType(FEED_TYPE);
        feed.setDescription(FEED_DESCRIPTION);
        feed.setLinks(Collections.singletonList(feedLink));
        feed.setPublishedDate(DateTimeUtilities.toDate(nowProvider.now()));
        feed.setEntries(feedItems);
        return feed;
    }

}
