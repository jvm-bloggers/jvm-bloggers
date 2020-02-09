package com.jvm_bloggers.frontend.admin_area.mailing;

import com.jvm_bloggers.entities.mailing_address.MailingAddress;
import com.jvm_bloggers.entities.mailing_address.MailingAddressRepository;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MailingAddressModel extends LoadableDetachableModel<MailingAddress> {

    @SpringBean
    private MailingAddressRepository mailingAddressRepository;

    private Long mailingAddressId;

    public MailingAddressModel(MailingAddress mailingAddress) {
        super(mailingAddress);
        Injector.get().inject(this);
        mailingAddressId = mailingAddress.getId();
    }

    @Override
    protected MailingAddress load() {
        return mailingAddressRepository
          .findById(mailingAddressId)
          .orElseThrow(() -> new RuntimeException("Mailing address with id " + mailingAddressId + " not found"));
    }
}
