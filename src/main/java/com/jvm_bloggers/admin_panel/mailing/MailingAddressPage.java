package com.jvm_bloggers.admin_panel.mailing;

import com.jvm_bloggers.admin_panel.panels.CustomPagingNavigator;
import com.jvm_bloggers.core.mailing.domain.MailingAddress;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("mailing-address")
@AuthorizeInstantiation(Roles.ADMIN)
public class MailingAddressPage extends AbstractMailingPage {

    @SpringBean
    private MailingAddressPageRequestHandler mailingAddressPageRequestHandler;

    public MailingAddressPage() {
        final DataView<MailingAddress> dataView = new DataView<MailingAddress>("pageable",
                mailingAddressPageRequestHandler) {
            @Override
            protected void populateItem(Item<MailingAddress> item) {
                MailingAddress modelObject = item.getModelObject();
                item.add(new Label("id", modelObject.getId()));
                item.add(new Label("address", modelObject.getAddress()));
            }
        };

        dataView.setItemsPerPage(defaultPaginationSize);
        add(dataView);
        add(new CustomPagingNavigator("navigator", dataView));
    }
}
