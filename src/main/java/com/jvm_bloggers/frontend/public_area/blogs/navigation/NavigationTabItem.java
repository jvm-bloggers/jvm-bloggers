package com.jvm_bloggers.frontend.public_area.blogs.navigation;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import org.apache.wicket.Page;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.Panel;

public class NavigationTabItem extends Panel {

    static final String ACTIVE_CSS_CLASS = "active";
    static final String LABEL_ID = "label";

    private final String label;

    private final Icon icon;

    private final Class<? extends Page> targetPageClass;

    private final Class<? extends Page> currentPageClass;

    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    public enum Icon {
        MALE("fa-male"),
        USERS("fa-black-tie"),
        PODCAST("fa-podcast"),
        YOUTUBE("fa-youtube-play");
        private String value;
    }

    public NavigationTabItem(String id,
                             String label,
                             Icon icon,
                             Class<? extends Page> targetPageClass,
                             Class<? extends Page> currentPageClass) {
        super(id);
        this.label = label;
        this.icon = icon;
        this.targetPageClass = targetPageClass;
        this.currentPageClass = currentPageClass;
    }

    @Override
    protected void onInitialize() {
        super.onInitialize();
        Link link = new BookmarkablePageLink<>("link", targetPageClass);
        link.add(new AttributeAppender("class", icon.value, " "));
        link.add(new Label(LABEL_ID, label));
        add(link);
        if (targetPageClass.equals(currentPageClass)) {
            add(new AttributeAppender("class", ACTIVE_CSS_CLASS, " "));
        }
    }
}
