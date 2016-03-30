package pl.tomaszdziurko.jvm_bloggers.view.login.helpers

import org.apache.wicket.markup.html.WebPage
import org.apache.wicket.markup.html.basic.Label
import org.apache.wicket.markup.repeater.Item
import org.apache.wicket.markup.repeater.data.DataView
import org.apache.wicket.markup.repeater.data.ListDataProvider
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomPagingNavigator
/**
 * Created by jborkowski on 28/03/16.
 */
class TestDataNavigationPage extends WebPage {

    public TestDataNavigationPage(final int itemPerPage, final int totalItems) {
        final DataView<Integer> dataView = new DataView<Integer>("data",
                new ListDataProvider<>(new ArrayList<>(1..totalItems))) {
            @Override
            protected void populateItem(Item<Integer> item) {
                final Integer number = item.getModelObject()
                item.add(new Label("id", number))
            }
        };

        dataView.setItemsPerPage(itemPerPage)
        add(dataView)
        add(new CustomPagingNavigator("navigator", dataView))
    }
}
