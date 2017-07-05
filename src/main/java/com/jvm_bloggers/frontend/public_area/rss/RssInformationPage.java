package com.jvm_bloggers.frontend.public_area.rss;

import com.jvm_bloggers.frontend.public_area.AbstractFrontendPage;
import com.jvm_bloggers.frontend.public_area.HomePage;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.request.Url;
import org.apache.wicket.request.cycle.RequestCycle;
import org.wicketstuff.annotation.mount.MountPath;

import static com.jvm_bloggers.core.rss.BlogPostsController.RSS_FEED_MAPPING;

@MountPath("rss")
public class RssInformationPage extends AbstractFrontendPage {

    public RssInformationPage() {
        add(new Label("rss_feed", String.format("%s%s", getRootUrl(), RSS_FEED_MAPPING)));
    }

    @Override
    protected String getPageTitle() {
        return "Kanał RSS - informacje";
    }

    private String getRootUrl() {
        return RequestCycle.get().getUrlRenderer().renderFullUrl(
            Url.parse(urlFor(HomePage.class, null).toString()));
    }
}
