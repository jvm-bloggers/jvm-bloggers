package pl.tomaszdziurko.jvm_bloggers.view.admin.blogs;

import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.Iterator;

/**
 * @author Mateusz Urbański <matek2305@gmail.com>.
 */
public interface RepositoryDataProvider<T> extends IDataProvider<T> {

    Page<T> page(Pageable pageable);

    long getPageSize();

    @Override
    default Iterator<? extends T> iterator(long first, long count) {
        int page = Long.valueOf(first / getPageSize()).intValue();
        int size = Long.valueOf(count).intValue();
        return page(new PageRequest(page, size)).iterator();
    }

    @Override
    default void detach() {

    }
}
