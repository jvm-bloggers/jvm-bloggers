package com.jvm_bloggers.frontend.admin_area.panels.tables;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.extensions.markup.html.repeater.data.table.DataTable;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.ISortableDataProvider;
import org.apache.wicket.extensions.markup.html.repeater.data.table.NoRecordsToolbar;

import java.util.List;

public class BootstrapTable<T, S> extends DataTable<T, S> {
    private static final long serialVersionUID = 1L;
    protected static final String BOOTSTRAP_TABLE_CLASSES =
        "table table-striped table-bordered table-hover dataTable no-footer";

    public BootstrapTable(final String id, final List<? extends IColumn<T, S>> columns,
                          final ISortableDataProvider<T, S> dataProvider, final int rowsPerPage) {
        super(id, columns, dataProvider, rowsPerPage);

        addTopToolbar(new AwesomeHeadersToolbar<>(this, dataProvider));

        addBottomToolbar(new NoRecordsToolbar(this));
        add(AttributeModifier.replace("cellspacing", "0"));
        add(AttributeModifier.append("class", BOOTSTRAP_TABLE_CLASSES));
    }
}
