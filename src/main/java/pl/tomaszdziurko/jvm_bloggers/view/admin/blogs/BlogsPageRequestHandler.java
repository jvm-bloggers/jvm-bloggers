package pl.tomaszdziurko.jvm_bloggers.view.admin.blogs;

import lombok.RequiredArgsConstructor;
import org.apache.wicket.model.IModel;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;

/**
 * @author Mateusz Urbański <matek2305@gmail.com>.
 */
@Component
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class BlogsPageRequestHandler implements RepositoryDataProvider<Blog> {

    private final BlogRepository blogRepository;

    @Override
    public Page<Blog> page(Pageable pageable) {
        return blogRepository.findAll(pageable);
    }

    @Override
    public long getPageSize() {
        return BlogsPage.BLOGS_PER_PAGE;
    }

    @Override
    public long size() {
        return blogRepository.count();
    }

    @Override
    public IModel<Blog> model(Blog blog) {
        return new BlogModel(blog);
    }
}
