package com.jvm_bloggers.frontend.admin_area.panels.tables;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.ISortState;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.ISortStateLocator;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.OrderByLink;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.border.Border;

class AwesomeOrderByBorder<S> extends Border {

    protected static final String SORT_ASCENDING_CSS_CLASSES = "fa-sort-asc";
    protected static final String SORT_DESCENDING_CSS_CLASSES = "fa-sort-desc";
    protected static final String SORT_NONE_CSS_CLASSES = "fa-sort";

    private static final long serialVersionUID = 1L;

    private final ISortStateLocator<S> stateLocator;

    private final S property;

    AwesomeOrderByBorder(final String id, final S property,
                         final ISortStateLocator<S> stateLocator) {
        super(id);

        this.stateLocator = stateLocator;
        this.property = property;

        OrderByLink<S> link = newOrderByLink("orderByLink", property, stateLocator);
        addToBorder(link);
    }

    protected OrderByLink<S> newOrderByLink(final String id, final S property,
                                            final ISortStateLocator<S> stateLocator) {
        return new OrderByLink<>(id, property, stateLocator) {
            private static final long serialVersionUID = 1L;

            @Override
            protected void onSortChanged() {
                AwesomeOrderByBorder.this.onSortChanged();
            }
        };
    }

    protected void onSortChanged() {
        // noop
    }

    @Override
    protected void onBeforeRender() {
        super.onBeforeRender();

        Label fa = new Label("fa");
        addToBorder(fa);

        final ISortState<S> sortState = stateLocator.getSortState();

        SortOrder dir = sortState.getPropertySortOrder(property);
        String cssClass;
        if (dir == SortOrder.ASCENDING) {
            cssClass = SORT_ASCENDING_CSS_CLASSES;
        } else if (dir == SortOrder.DESCENDING) {
            cssClass = SORT_DESCENDING_CSS_CLASSES;
        } else {
            cssClass = SORT_NONE_CSS_CLASSES;
        }

        fa.add(AttributeModifier.append("class", cssClass));
    }
}
