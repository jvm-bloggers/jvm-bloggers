package com.jvm_bloggers.frontend.common_components;

import com.jvm_bloggers.core.utils.LinkUtils;
import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedPost;
import com.jvm_bloggers.utils.CleanString;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.panel.Panel;

public class PublishedBlogPostLink extends Panel {

    public PublishedBlogPostLink(String id, PublishedPost post) {
        super(id);
        boolean authorHasTwitterHandle = post.getAuthorTwitterHandle() != null;
        String title = CleanString.clean(post.getTitle());
        add(new ExternalLink("postLink", post.getUrl(), title));
        Label authorLabel = new Label("authorLabel", post.getAuthorName());
        authorLabel.setVisible(!authorHasTwitterHandle);
        add(authorLabel);
        add(new ExternalLink(
                "authorTwitterLink",
                LinkUtils.getFullTwitterAccountUrl(post.getAuthorTwitterHandle()),
                post.getAuthorName()
            ).setVisible(authorHasTwitterHandle)
        );
    }
}
