package com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel;

import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedPost;
import com.jvm_bloggers.frontend.common_components.PublishedBlogPostLink;
import io.vavr.collection.Seq;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;

class BlogPostLinksSection extends Panel {

    BlogPostLinksSection(String id, String heading, Seq<PublishedPost> blogPosts) {
        super(id);
        add(new Label("sectionHeading", heading));
        add(new ListView<>("postItems", blogPosts.toJavaList()) {
            @Override
            protected void populateItem(ListItem<PublishedPost> item) {
                PublishedPost post = item.getModelObject();
                item.add(new PublishedBlogPostLink("postLink", post));
            }
        });
        setVisible(!blogPosts.isEmpty());
    }

}
