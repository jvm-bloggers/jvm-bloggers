package com.jvm_bloggers.frontend.admin_area.blogs;

import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog.BlogRepository;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

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
        return blogRepository
          .findById(blogId)
          .orElseThrow(() -> new RuntimeException("Blog with id " + blogId + " not found"));
    }

}
