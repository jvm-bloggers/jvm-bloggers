package com.jvm_bloggers.core.rss;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Preconditions;
import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository;
import com.jvm_bloggers.utils.DateTimeUtilities;
import com.jvm_bloggers.utils.NowProvider;
import com.rometools.rome.feed.synd.SyndEntry;
import com.rometools.rome.feed.synd.SyndEntryImpl;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.feed.synd.SyndFeedImpl;
import com.rometools.rome.feed.synd.SyndLink;
import com.rometools.rome.feed.synd.SyndLinkImpl;

import io.vavr.collection.Seq;

import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.Collections;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

@Slf4j
@Service
public class IssuesRssFeedProducer {

    @VisibleForTesting
    static final String FEED_TITLE = "JVM Bloggers Issues";
    @VisibleForTesting
    static final String FEED_TYPE = "atom_1.0";
    private static final String SELF_REL = "self";
    @VisibleForTesting
    static final String FEED_DESCRIPTION = "JVM Bloggers Issues feed. "
        + "Weekly aggregation of JVM related blog posts from Poland.";

    private final NewsletterIssueRepository issueRepository;
    private final LinkGenerator linkGenerator;
    private final NowProvider nowProvider;
    private final String appName;

    public IssuesRssFeedProducer(
        NewsletterIssueRepository issueRepository,
        LinkGenerator linkGenerator,
        NowProvider nowProvider,
        @Value("${info.app.name}") String appName) {
        this.issueRepository = issueRepository;
        this.linkGenerator = linkGenerator;
        this.nowProvider = nowProvider;
        this.appName = appName;
    }

    public SyndFeed getRss(String feedUrl, Integer limit) {
        Preconditions.checkArgument(isNotBlank(feedUrl), "feedUrl parameter cannot be blank");

        final Pageable pageRequest = PageRequest.of(0, limit > 0 ? limit : Integer.MAX_VALUE);
        Seq<NewsletterIssue> issues = issueRepository.findByOrderByPublishedDateDesc(pageRequest);
        final Seq<SyndEntry> feedItems = issues
            .map(this::toRssEntry);

        return buildFeed(feedItems, feedUrl);
    }

    private SyndEntry toRssEntry(NewsletterIssue issue) {
        final SyndEntry rssEntry = new SyndEntryImpl();
        rssEntry.setTitle("Wydanie #" + issue.getIssueNumber());
        rssEntry.setLink(linkGenerator.generateIssueLink(issue.getIssueNumber()));
        rssEntry.setAuthor(appName);
        rssEntry.setPublishedDate(DateTimeUtilities.toDate(issue.getPublishedDate()));
        rssEntry.setUri(issue.getIssueNumber().toString());

        return rssEntry;
    }

    private SyndFeed buildFeed(final Seq<SyndEntry> feedItems, String requestedUrlString) {
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
        feed.setEntries(feedItems.toJavaList());
        return feed;
    }
}
