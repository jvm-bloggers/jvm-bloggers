package pl.tomaszdziurko.jvm_bloggers.view.panels;

import org.apache.wicket.markup.html.navigation.paging.IPageable;
import org.apache.wicket.markup.html.navigation.paging.IPagingLabelProvider;
import org.apache.wicket.markup.html.navigation.paging.PagingNavigator;

public class CustomPagingNavigator extends PagingNavigator {

    public CustomPagingNavigator(String id, IPageable pageable) {
        super(id, pageable);
    }

    public CustomPagingNavigator(String id, IPageable pageable,
                                 IPagingLabelProvider labelProvider) {
        super(id, pageable, labelProvider);
    }
}

