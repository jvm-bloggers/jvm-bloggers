package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import com.google.common.annotations.VisibleForTesting;
import com.rometools.rome.feed.synd.SyndContentImpl;
import com.rometools.rome.feed.synd.SyndEntry;
import com.rometools.rome.feed.synd.SyndEntryImpl;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.feed.synd.SyndFeedImpl;
import com.rometools.rome.feed.synd.SyndLinkImpl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.util.StopWatch;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;
import pl.tomaszdziurko.jvm_bloggers.utils.UriUtmComponentsBuilder;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities.toDate;

@Service
@CacheConfig(cacheNames = AggregatedRssFeedProducer.RSS_CACHE)
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
@Slf4j
public class AggregatedRssFeedProducer {

    public static final String RSS_CACHE = "Aggregated RSS feed cache";
    private static final String RSS_CACHE_KEY = "'rss_cache_key'";
    private static final String SELF_REL = "self";

    @VisibleForTesting
    static final String FEED_DESCRIPTION = "JVMBloggers aggregated feed";
    @VisibleForTesting
    static final String FEED_TITLE = "JVMBloggers";
    @VisibleForTesting
    static final String FEED_TYPE = "atom_1.0";

    private static final String UTM_MEDIUM = "RSS";
    private static final String UTM_CAMPAIGN = "RSS";


    private final BlogPostRepository blogPostRepository;
    private final NowProvider nowProvider;

    @Cacheable(key = RSS_CACHE_KEY)
    public SyndFeed getRss(String requestedUrlString) {

        final StopWatch stopWatch = new StopWatch();
        log.debug("Building aggregated RSS feed...");
        stopWatch.start();

        final List<BlogPost> approvedPosts =
            blogPostRepository.findByApprovedTrueOrderByPublishedDateDesc();
        final List<SyndEntry> feedItems = approvedPosts.stream()
            .map(this::toRssEntry)
            .collect(Collectors.toList());

        final SyndFeed feed = buildFeed(feedItems, requestedUrlString);

        stopWatch.stop();
        log.info("Total {} feed entries produced in {}ms", feedItems.size(),
            stopWatch.getTotalTimeMillis());

        return feed;
    }

    private SyndEntry toRssEntry(BlogPost post) {
        final SyndEntry rssEntry = new SyndEntryImpl();
        rssEntry.setTitle(post.getTitle());
        rssEntry.setLink(addUtmComponents(post.getUrl()));
        rssEntry.setAuthor(post.getBlog().getAuthor());
        rssEntry.setPublishedDate(toDate(post.getPublishedDate()));
        rssEntry.setUri(post.getUid());

        final String description = post.getDescription();
        if (isNotBlank(description)) {
            final SyndContentImpl descriptionContent = new SyndContentImpl();
            descriptionContent.setValue(description);
            rssEntry.setDescription(descriptionContent);
        }

        return rssEntry;
    }

    private String addUtmComponents(String url) {
        return UriUtmComponentsBuilder.fromHttpUrl(url)
            .withSource(UriUtmComponentsBuilder.DEFAULT_UTM_SOURCE)
            .withMedium(UTM_MEDIUM)
            .withCampaign(UTM_CAMPAIGN)
            .build();
    }

    private SyndFeed buildFeed(final List<SyndEntry> feedItems, String requestedUrlString) {
        final SyndLinkImpl feedLink = new SyndLinkImpl();
        feedLink.setRel(SELF_REL);
        feedLink.setHref(requestedUrlString);
        final SyndFeed feed = new SyndFeedImpl();
        feed.setUri(FEED_TITLE);
        feed.setTitle(FEED_TITLE);
        feed.setFeedType(FEED_TYPE);
        feed.setDescription(FEED_DESCRIPTION);
        feed.setLinks(Arrays.asList(feedLink));
        feed.setPublishedDate(toDate(nowProvider.now()));
        feed.setEntries(feedItems);
        return feed;
    }

}
