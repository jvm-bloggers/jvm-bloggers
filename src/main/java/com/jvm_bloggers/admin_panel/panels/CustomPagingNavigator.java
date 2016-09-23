package com.jvm_bloggers.admin_panel.panels;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.LoopItem;
import org.apache.wicket.markup.html.navigation.paging.IPageable;
import org.apache.wicket.markup.html.navigation.paging.IPagingLabelProvider;
import org.apache.wicket.markup.html.navigation.paging.PagingNavigation;
import org.apache.wicket.markup.html.navigation.paging.PagingNavigator;
import org.apache.wicket.model.Model;

public class CustomPagingNavigator extends PagingNavigator {

    public CustomPagingNavigator(String id, IPageable pageable) {
        super(id, pageable);
        add(new Label("total", getTotalPagesText()));
    }

    public CustomPagingNavigator(String id, IPageable pageable,
                                 IPagingLabelProvider labelProvider) {
        super(id, pageable, labelProvider);
    }

    @Override
    protected PagingNavigation newNavigation(String id, IPageable pageable,
                                             IPagingLabelProvider labelProvider) {
        return new PagingNavigation(id, pageable, labelProvider) {
            @Override
            protected LoopItem newItem(int iteration) {
                final LoopItem item = super.newItem(iteration);
                final long pageIdx = getStartIndex() + iteration;

                if (pageable.getCurrentPage() == pageIdx) {
                    item.add(new AttributeModifier("class", Model.of("active")));
                }

                return item;
            }
        };
    }

    private String getTotalPagesText() {
        return String.format("Total pages: %d", getPageable().getPageCount());
    }
}

