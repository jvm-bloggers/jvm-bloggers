package com.jvm_bloggers.frontend.admin_area.panels.tables;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.ISortStateLocator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.DataTable;
import org.apache.wicket.extensions.markup.html.repeater.data.table.HeadersToolbar;
import org.apache.wicket.markup.html.WebMarkupContainer;

class AwesomeHeadersToolbar<S> extends HeadersToolbar<S> {

    /**
     * Constructor
     *
     * @param table data table this toolbar will be attached to
     */
    AwesomeHeadersToolbar(
        DataTable table,
        ISortStateLocator stateLocator) {
        super(table, stateLocator);
    }

    @Override
    protected WebMarkupContainer newSortableHeader(final String headerId, final S property,
                                                   final ISortStateLocator<S> locator) {
        return new AwesomeOrderByBorder<S>(headerId, property, locator);
    }

}
