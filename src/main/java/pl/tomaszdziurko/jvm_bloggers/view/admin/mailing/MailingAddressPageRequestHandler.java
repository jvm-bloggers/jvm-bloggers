package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.mailing.domain.MailingAddress;
import pl.tomaszdziurko.jvm_bloggers.mailing.domain.MailingAddressRepository;
import pl.tomaszdziurko.jvm_bloggers.view.admin.PaginationConfiguration;

import java.util.Iterator;

@Component
@Slf4j
@AllArgsConstructor(onConstructor = @__(@Autowired))
public class MailingAddressPageRequestHandler implements IDataProvider<MailingAddress> {

    private final PaginationConfiguration paginationConfiguration;

    private final MailingAddressRepository mailingAddressRepository;

    @Override
    public Iterator<? extends MailingAddress> iterator(long first, long count) {
        int page = Long.valueOf(first / paginationConfiguration.getDefaultPageSize()).intValue();
        return mailingAddressRepository
                .findAllByOrderByAddressAsc(new PageRequest(page,
                        paginationConfiguration.getDefaultPageSize())
                ).iterator();
    }

    @Override
    public long size() {
        return mailingAddressRepository.count();
    }

    @Override
    public IModel<MailingAddress> model(MailingAddress mailingAddress) {
        return new MailingAddressModel(mailingAddress);
    }

    @Override
    public void detach() {

    }
}
