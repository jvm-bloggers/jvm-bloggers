package com.jvm_bloggers.frontend.admin_area.mailing;

import com.jvm_bloggers.domain.query.unread_varia_suggestion.UnreadVariaSuggestion;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

import java.util.Iterator;

@Slf4j
@RequiredArgsConstructor
public class VariaSuggestionRequestHandler implements IDataProvider<UnreadVariaSuggestion> {

    private final MailingPageBackingBean backingBean;
    private final int pageSize;

    @Override
    public Iterator<? extends UnreadVariaSuggestion> iterator(long first, long count) {
        log.debug("Refreshing data, first {}, count {}", first, count);
        int page = (int) (first / pageSize);
        long start = System.currentTimeMillis();
        Iterator<UnreadVariaSuggestion> iterator = backingBean.findUnreadSuggestions(page, pageSize)
            .iterator();
        long stop = System.currentTimeMillis();
        log.debug("Iterator() execution time = " + (stop - start) + " ms");
        return iterator;
    }

    @Override
    public long size() {
        long start = System.currentTimeMillis();
        long count = backingBean.countUnreadVariaSuggestion();
        long stop = System.currentTimeMillis();
        log.debug("Size() execution time = " + (stop - start) + " ms");
        return count;
    }

    @Override
    public IModel<UnreadVariaSuggestion> model(UnreadVariaSuggestion object) {
        return Model.of(object);
    }
}
