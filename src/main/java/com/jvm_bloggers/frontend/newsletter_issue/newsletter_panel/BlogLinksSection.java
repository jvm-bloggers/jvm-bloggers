package com.jvm_bloggers.frontend.newsletter_issue.newsletter_panel;

import com.jvm_bloggers.frontend.newsletter_issue.BlogDto;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;

import java.util.List;

public class BlogLinksSection extends Panel {

    public BlogLinksSection(String id, List<BlogDto> blogs) {
        super(id);
        add(new Label("sectionHeading", "Nowe blogi i kanały video"));
        add(new ListView<BlogDto>("blogItems", blogs) {
            @Override
            protected void populateItem(ListItem<BlogDto> item) {
                BlogDto blog = item.getModelObject();
                item.add(new ExternalLink("blogLink", blog.url, blog.author));
            }
        });

        setVisible(!blogs.isEmpty());
    }
}
