package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.mailing.domain.MailingAddress;
import pl.tomaszdziurko.jvm_bloggers.mailing.domain.MailingAddressRepository;

import java.util.Iterator;

@Component
@Slf4j
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class MailingAddressPageRequestHandler implements IDataProvider<MailingAddress> {

    @Value("${items.pagination.size}")
    private int paginationLimit;

    private final MailingAddressRepository mailingAddressRepository;

    @Override
    public Iterator<? extends MailingAddress> iterator(long first, long count) {
        int page = Long.valueOf(first / paginationLimit).intValue();
        return mailingAddressRepository
                .findAllByOrderByAddressAsc(new PageRequest(page, paginationLimit))
                .iterator();
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
