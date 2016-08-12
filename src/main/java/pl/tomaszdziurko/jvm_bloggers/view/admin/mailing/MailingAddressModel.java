package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import pl.tomaszdziurko.jvm_bloggers.mailing.domain.MailingAddress;
import pl.tomaszdziurko.jvm_bloggers.mailing.domain.MailingAddressRepository;

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
        return mailingAddressRepository.findOne(mailingAddressId);
    }
}
