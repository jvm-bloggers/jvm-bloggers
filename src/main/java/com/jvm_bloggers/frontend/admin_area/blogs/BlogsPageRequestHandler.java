package com.jvm_bloggers.frontend.admin_area.blogs;

import com.jvm_bloggers.core.utils.WicketToSpringSortingConverter;
import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog.BlogRepository;
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration;

import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.springframework.data.domain.PageRequest;

import java.util.Iterator;

public class BlogsPageRequestHandler extends SortableDataProvider<Blog, String> {

    @SpringBean
    private PaginationConfiguration paginationConfiguration;

    @SpringBean
    private BlogRepository blogRepository;

    BlogsPageRequestHandler() {
        Injector.get().inject(this);
        setSort(new SortParam<>("author", true));
    }

    @Override
    public Iterator<? extends Blog> iterator(long first, long count) {
        int page = (int) (first / paginationConfiguration.getDefaultPageSize());

        return blogRepository
            .findAll(PageRequest.of(page,
                paginationConfiguration.getDefaultPageSize(),
                WicketToSpringSortingConverter.convert(getSort()).getOrNull())
            )
            .iterator();
    }

    @Override
    public long size() {
        return blogRepository.count();
    }

    @Override
    public IModel<Blog> model(Blog blog) {
        return new BlogModel(blog);
    }

    @Override
    public void detach() {

    }
}
