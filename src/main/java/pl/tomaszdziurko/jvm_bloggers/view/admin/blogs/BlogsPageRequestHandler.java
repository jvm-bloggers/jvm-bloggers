package pl.tomaszdziurko.jvm_bloggers.view.admin.blogs;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;

import java.util.Iterator;

@Component
@NoArgsConstructor
@AllArgsConstructor(onConstructor = @__(@Autowired))
public class BlogsPageRequestHandler implements IDataProvider<Blog> {

    @SpringBean
    private BlogRepository blogRepository;

    @Override
    public Iterator<? extends Blog> iterator(long first, long count) {
        int page = Long.valueOf(first / BlogsPage.BLOGS_PER_PAGE).intValue();
        int size = Long.valueOf(count).intValue();
        return blogRepository.findAllByOrderByDateAddedDesc(new PageRequest(page, size)).iterator();
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
