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

    public InfinitePaginationPanel(String id, Component itemParent, DataView dataView) {
        super(id);
        setOutputMarkupId(true);

        callbackUrl = new Model<>();
        pager = new AjaxPagingNavigator("pager", dataView);
        Component nextLink = new Label("next-page").add(new AttributeModifier("href", callbackUrl));

        add(pager);
        add(nextLink);
        add(newInfiniteScrollingBehavior(pager, nextLink, itemParent));
    }

    protected InfiniteScrollingBehavior newInfiniteScrollingBehavior(final Component pager,
                                                                     final Component nextLink,
                                                                     final Component itemParent) {
        final InfiniteScrollingBehavior scrollingBehavior = new InfiniteScrollingBehavior();
        scrollingBehavior.setNavSelector(pager);
        scrollingBehavior.setItemSelector(itemParent, ".item");
        scrollingBehavior.setNextSelector(nextLink);
        scrollingBehavior.extraScrollPx(20);
        scrollingBehavior.loadingMsgText("loading...");
        scrollingBehavior.loadingFinishedMsg("completed!");

        return scrollingBehavior;
    }

    @Override
    protected void onConfigure() {
        super.onConfigure();
        pager.add(new AttributeModifier("style", "position:absolute;left:-9999px;"));
        for (Behavior behavior : pager.get("next").getBehaviors()) {
            if (behavior instanceof AjaxPagingNavigationBehavior) {
                callbackUrl.setObject(((AjaxPagingNavigationBehavior) behavior)
                    .getCallbackUrl().toString());
                break;
            }
        }
    }
}
