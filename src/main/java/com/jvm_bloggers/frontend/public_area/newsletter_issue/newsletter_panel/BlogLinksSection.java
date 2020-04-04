package com.jvm_bloggers.frontend.public_area.newsletter_issue.newsletter_panel;

import com.jvm_bloggers.domain.query.published_newsletter_issue.NewlyAddedBlog;

import io.vavr.collection.Seq;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;

class BlogLinksSection extends Panel {

    BlogLinksSection(String id, Seq<NewlyAddedBlog> blogs) {
        super(id);
        add(new Label("sectionHeading", "Nowe blogi i kanały video"));
        add(new ListView<>("blogItems", blogs.toJavaList()) {
            @Override
            protected void populateItem(ListItem<NewlyAddedBlog> item) {
                NewlyAddedBlog blog = item.getModelObject();
                item.add(new ExternalLink("blogLink", blog.getUrl(), blog.getAuthor()));
            }
        });
        setVisible(!blogs.isEmpty());
    }

}
