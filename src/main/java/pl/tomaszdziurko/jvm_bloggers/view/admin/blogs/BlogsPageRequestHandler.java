package pl.tomaszdziurko.jvm_bloggers.view.admin.blogs;

import lombok.RequiredArgsConstructor;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;

import java.util.Iterator;

@Component
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class BlogsPageRequestHandler implements IDataProvider<Blog> {

    @Value("${items.pagination.size}")
    private int paginationLimit;

    @SpringBean
    private final BlogRepository blogRepository;

    @Override
    public Iterator<? extends Blog> iterator(long first, long count) {
        int page = Long.valueOf(first / paginationLimit).intValue();
        return blogRepository.findAllByOrderByAuthorAsc(new PageRequest(page, paginationLimit)).iterator();
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
