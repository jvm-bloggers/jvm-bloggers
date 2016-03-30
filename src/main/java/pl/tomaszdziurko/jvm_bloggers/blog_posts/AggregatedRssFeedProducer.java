package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Preconditions;
import com.rometools.rome.feed.synd.SyndContentImpl;
import com.rometools.rome.feed.synd.SyndEntry;
import com.rometools.rome.feed.synd.SyndEntryImpl;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.feed.synd.SyndFeedImpl;
import com.rometools.rome.feed.synd.SyndLink;
import com.rometools.rome.feed.synd.SyndLinkImpl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
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
     * @return Aggregated RSS feed for approved blog posts ordered by publication date
     */
    @Cacheable
    public SyndFeed getRss(String feedUrl, int limit) {

        Preconditions.checkArgument(
            StringUtils.isNotBlank(feedUrl), "feedUrl parameter cannot be blank");

        final StopWatch stopWatch = new StopWatch();
        log.debug("Building aggregated RSS feed...");
        stopWatch.start();

        final Pageable pageRequest = new PageRequest(0, limit > 0 ? limit : Integer.MAX_VALUE);
        final List<BlogPost> approvedPosts =
            blogPostRepository.findByApprovedTrueOrderByPublishedDateDesc(pageRequest);
        final List<SyndEntry> feedItems = approvedPosts.stream()
            .map(this::toRssEntry)
            .collect(Collectors.toList());

        final SyndFeed feed = buildFeed(feedItems, feedUrl);

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
        final SyndLink feedLink = new SyndLinkImpl();
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
