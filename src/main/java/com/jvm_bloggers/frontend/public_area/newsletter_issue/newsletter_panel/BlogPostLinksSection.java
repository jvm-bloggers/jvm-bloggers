package com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel;

import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedPost;

import javaslang.collection.Seq;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;

class BlogPostLinksSection extends Panel {

    private static final String TWITTER_HOME_URL = "https://twitter.com/";

    BlogPostLinksSection(String id, String heading, Seq<PublishedPost> blogPosts) {
        super(id);
        add(new Label("sectionHeading", heading));
        add(new ListView<PublishedPost>("postItems", blogPosts.toJavaList()) {
            @Override
            protected void populateItem(ListItem<PublishedPost> item) {
                PublishedPost post = item.getModelObject();
                boolean authorHasTwitterHandle = post.getAuthorTwitterHandle() != null;
                item.add(new ExternalLink("postLink", post.getUrl(), post.getTitle()));
                Label authorLabel = new Label("authorLabel", post.getAuthorName());
                authorLabel.setVisible(!authorHasTwitterHandle);
                item.add(authorLabel);
                item.add(
                    new ExternalLink(
                        "authorTwitterLink",
                        TWITTER_HOME_URL + post.getAuthorTwitterHandle(),
                        post.getAuthorName()
                    ).setVisible(authorHasTwitterHandle));
            }
        });
        setVisible(!blogPosts.isEmpty());
    }

}
