package com.jvm_bloggers.frontend.admin_area.bot_clicks_detector;

import com.jvm_bloggers.entities.click.PostClicksCountByUserAgent;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;

public class PostClicksCountByUserAgentForListingPopulator {

    static final String TITLE_ID = "title";
    static final String AUTHOR_ID = "author";
    static final String AGGREGATOR_VALUE_ID = "aggregatorValue";
    static final String COUNTER_ID = "counter";

    public void populateItem(final ListItem<PostClicksCountByUserAgent> item) {
        PostClicksCountByUserAgent
            postClicksCountByUserAgentForListing =
            item.getModelObject();
        item.add(new Label(TITLE_ID, postClicksCountByUserAgentForListing.getTitle()));
        item.add(new Label(AUTHOR_ID, postClicksCountByUserAgentForListing.getAuthor()));
        item.add(
            new Label(AGGREGATOR_VALUE_ID, postClicksCountByUserAgentForListing.getUserAgent()));
        item.add(new Label(COUNTER_ID, postClicksCountByUserAgentForListing.getCounter()));
    }
}
