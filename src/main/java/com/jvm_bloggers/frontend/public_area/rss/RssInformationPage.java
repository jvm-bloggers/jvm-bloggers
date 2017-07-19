package com.jvm_bloggers.frontend.public_area.rss;

import com.jvm_bloggers.frontend.public_area.AbstractFrontendPage;
import com.jvm_bloggers.frontend.public_area.HomePage;

import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.request.Url;
import org.apache.wicket.request.cycle.RequestCycle;
import org.wicketstuff.annotation.mount.MountPath;

import java.util.Arrays;

import static com.jvm_bloggers.core.rss.BlogPostsController.RSS_FEED_MAPPING;

@MountPath("rss")
public class RssInformationPage extends AbstractFrontendPage {
    private static final String FEED_URL_PATTERN = "%s%s";

    private final String feedUrl;

    public RssInformationPage() {
        this.feedUrl = String.format(FEED_URL_PATTERN, getRootUrl(), RSS_FEED_MAPPING);

        add(new ExternalLink("rss_feed_url", feedUrl, feedUrl));

        Arrays.stream(FeedUrl.values())
            .forEach(e -> add(getExternalLink(e)));
    }

    @Override
    protected String getPageTitle() {
        return "Kanał RSS - informacje";
    }

    private String getRootUrl() {
        return RequestCycle.get().getUrlRenderer().renderFullUrl(
            Url.parse(urlFor(HomePage.class, null).toString()));
    }

    private String getAbsoluteUrl(String parameter) {
        return String.format(FEED_URL_PATTERN, feedUrl, parameter);
    }

    private ExternalLink getExternalLink(FeedUrl feedUrl) {
        String url = getAbsoluteUrl(feedUrl.getParameter());
        return new ExternalLink(feedUrl.getId(), url, url);
    }

    private enum FeedUrl {
        XML("rss_feed_xml", ".xml"),
        JSON("rss_feed_json", ".json"),
        LIMIT("rss_feed_limit", "?limit=10"),
        EXCLUDE_AUTHORS("rss_feed_exclude", "?excludedAuthors=Tomasz Dziurko,Adam Warski"),
        COMPLEX("rss_feed_complex", ".json?limit=2&excludedAuthors=Tomasz Dziurko");

        private final String id;
        private final String parameter;

        FeedUrl(String id, String parameter) {
            this.id = id;
            this.parameter = parameter;
        }

        String getId() {
            return id;
        }

        String getParameter() {
            return parameter;
        }
    }
}
