package com.jvm_bloggers.frontend.public_area.blogs.timeline_panel;

import com.jvm_bloggers.entities.blog_post.BlogPost;

import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.panel.Panel;

import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_TIME_FORMATTER;

public class BlogPostDetailsPanel extends Panel {

    static final String LINK_ID = "link";
    static final String PUBLISHED_DATE_ID = "publishedDate";

    public BlogPostDetailsPanel(String id, String orientationClass, BlogPost blogPost) {
        super(id);
        add(new AttributeAppender("class", orientationClass, " "));
        add(new ExternalLink(LINK_ID, blogPost.getUrl(), blogPost.getTitle()));
        add(new Label(PUBLISHED_DATE_ID, blogPost.getPublishedDate().format(DATE_TIME_FORMATTER)));
    }
}
