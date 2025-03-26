package com.jvm_bloggers.frontend.admin_area.bot_clicks_detector;

import com.jvm_bloggers.entities.click.PostClicksCountByIpAddress;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;

public class PostClicksCountByIpAddressForListingPopulator {

    public static final String TITLE_ID = "title";
    public static final String AUTHOR_ID = "author";
    public static final String AGGREGATOR_VALUE_ID = "aggregatorValue";
    public static final String COUNTER_ID = "counter";

    public void populateItem(final ListItem<PostClicksCountByIpAddress> item) {
        PostClicksCountByIpAddress
            postClicksCountByAggregatorForListing =
            item.getModelObject();
        item.add(new Label(TITLE_ID, postClicksCountByAggregatorForListing.getTitle()));
        item.add(new Label(AUTHOR_ID, postClicksCountByAggregatorForListing.getAuthor()));
        item.add(
            new Label(AGGREGATOR_VALUE_ID, postClicksCountByAggregatorForListing.getIpAddress()));
        item.add(new Label(COUNTER_ID, postClicksCountByAggregatorForListing.getCounter()));
    }
}
