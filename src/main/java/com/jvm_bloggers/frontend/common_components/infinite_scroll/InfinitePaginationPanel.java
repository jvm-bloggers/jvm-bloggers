package com.jvm_bloggers.frontend.common_components.infinite_scroll;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigationBehavior;
import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigator;
import org.apache.wicket.behavior.Behavior;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.navigation.paging.PagingNavigator;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.model.Model;

public class InfinitePaginationPanel extends Panel {

    private final PagingNavigator pager;

    private final Model<String> callbackUrl;

    public InfinitePaginationPanel(String id, DataView dataView) {
        super(id);
        setOutputMarkupId(true);

        callbackUrl = new Model<>();
        pager = new AjaxPagingNavigator("pager", dataView);
        long totalPageCountValue = pager.getPageable().getPageCount();
        Component nextLink = new Label("next-page").add(new AttributeModifier("href", callbackUrl));
        Component totalPageCount = new Label("total-page-count", totalPageCountValue);

        add(pager);
        add(nextLink);
        add(totalPageCount);
        if (totalPageCountValue > 1) {
            add(newInfiniteScrollingBehavior(nextLink, totalPageCount));
        }
    }

    protected InfiniteScrollingBehavior newInfiniteScrollingBehavior(final Component nextLink,
                                                           final Component totalPageCount) {
        final InfiniteScrollingBehavior scrollingBehavior = new InfiniteScrollingBehavior();
        scrollingBehavior.setNextSelector(nextLink);
        scrollingBehavior.setTotalPageCountSelector(totalPageCount);

        return scrollingBehavior;
    }

    @Override
    protected void onConfigure() {
        super.onConfigure();
        for (Behavior behavior : pager.get("next").getBehaviors()) {
            if (behavior instanceof AjaxPagingNavigationBehavior) {
                callbackUrl.setObject(((AjaxPagingNavigationBehavior) behavior)
                    .getCallbackUrl().toString());
                break;
            }
        }
    }
}
