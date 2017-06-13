package com.jvm_bloggers.frontend.common_components;

import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedPost;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.panel.Panel;

public class PublishedBlogPostLink extends Panel {

    private static final String TWITTER_HOME_URL = "https://twitter.com/";

    public PublishedBlogPostLink(String id, PublishedPost post) {
        super(id);
        boolean authorHasTwitterHandle = post.getAuthorTwitterHandle() != null;
        add(new ExternalLink("postLink", post.getUrl(), post.getTitle()));
        Label authorLabel = new Label("authorLabel", post.getAuthorName());
        authorLabel.setVisible(!authorHasTwitterHandle);
        add(authorLabel);
        add(new ExternalLink(
                "authorTwitterLink",
                TWITTER_HOME_URL + post.getAuthorTwitterHandle(),
                post.getAuthorName()
            ).setVisible(authorHasTwitterHandle)
        );
    }
}
