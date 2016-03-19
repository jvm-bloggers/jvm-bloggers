package pl.tomaszdziurko.jvm_bloggers.view.admin.blogs;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;

public class BlogModel extends LoadableDetachableModel<Blog> {

    @SpringBean
    private BlogRepository blogRepository;

    private final Long blogId;

    public BlogModel(Blog blog) {
        super(blog);
        Injector.get().inject(this);
        blogId = blog.getId();
    }

    @Override
    protected Blog load() {
        return blogRepository.findOne(blogId);
    }
}
