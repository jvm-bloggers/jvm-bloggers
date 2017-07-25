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

        Arrays.stream(FeedParameter.values())
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

    private ExternalLink getExternalLink(FeedParameter feedParameter) {
        String url = getAbsoluteUrl(feedParameter.getParameterExample());
        return new ExternalLink(feedParameter.getComponentId(), url, url);
    }

    private enum FeedParameter {
        XML("rss_feed_xml", ".xml"),
        JSON("rss_feed_json", ".json"),
        LIMIT("rss_feed_limit", "?limit=10"),
        EXCLUDE_AUTHORS("rss_feed_exclude", "?excludedAuthors=Tomasz Dziurko,Adam Warski"),
        COMPLEX("rss_feed_complex", ".json?limit=2&excludedAuthors=Tomasz Dziurko");

        private final String componentId;
        private final String parameterExample;

        FeedParameter(String componentId, String parameterExample) {
            this.componentId = componentId;
            this.parameterExample = parameterExample;
        }

        String getComponentId() {
            return componentId;
        }

        String getParameterExample() {
            return parameterExample;
        }
    }
}
