package com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel;

import com.jvm_bloggers.frontend.public_area.newsletter_issue.BlogPostDto;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;

import java.util.List;

public class BlogPostLinksSection extends Panel {

    private static final String TWITTER_HOME_URL = "https://twitter.com/";

    public BlogPostLinksSection(String id, String heading, List<BlogPostDto> blogPosts) {
        super(id);
        add(new Label("sectionHeading", heading));
        add(new ListView<BlogPostDto>("postItems", blogPosts) {
            @Override
            protected void populateItem(ListItem<BlogPostDto> item) {
                BlogPostDto post = item.getModelObject();
                boolean authorHasTwitterHandle = post.authorTwitterHandle != null;
                item.add(new ExternalLink("postLink", post.url, post.title));
                Label authorLabel = new Label("authorLabel", post.authorName);
                authorLabel.setVisible(!authorHasTwitterHandle);
                item.add(authorLabel);
                item.add(
                    new ExternalLink(
                        "authorTwitterLink",
                        TWITTER_HOME_URL + post.authorTwitterHandle,
                        post.authorName
                    ).setVisible(authorHasTwitterHandle));
            }
        });

        setVisible(!blogPosts.isEmpty());
    }
}
