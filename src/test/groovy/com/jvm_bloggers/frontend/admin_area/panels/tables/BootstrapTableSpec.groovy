package com.jvm_bloggers.frontend.admin_area.panels.tables

import com.jvm_bloggers.MockSpringContextAwareSpecification
import org.apache.wicket.extensions.markup.html.repeater.data.table.ISortableDataProvider
import org.apache.wicket.markup.Markup

class BootstrapTableSpec extends MockSpringContextAwareSpecification {

    @Override
    protected void setupContext() {
    }

    def "Table should contain bootstrap classes"() {
        given:
            def markup = Markup.of("<table wicket:id=\"tableId\"></table>")
            def table = new BootstrapTable("tableId", Collections.emptyList(), Mock(ISortableDataProvider), 10)

        when:
            tester.startComponentInPage(table, markup)

        then:
            tester.assertContains("<table wicket:id=\"tableId\" cellspacing=\"0\" class=\"" + BootstrapTable.BOOTSTRAP_TABLE_CLASSES)

    }
}
